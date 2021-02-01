use std::{
    future::Future,
    pin::Pin,
    sync::{
        atomic::{self, AtomicBool},
        Arc,
    },
    task::{Context, Poll, Waker},
};

use futures::task::noop_waker;
use parking_lot::{MappedMutexGuard, Mutex, MutexGuard, RwLock, RwLockUpgradableReadGuard};

pub use moxie2_macros::{local_slots_interface, nested_slots};

pub struct StateBuilder {
    control: Arc<RwLock<RevisionControl>>,
}

impl StateBuilder {
    pub fn new() -> Self {
        StateBuilder {
            control: Arc::new(RwLock::new(RevisionControl::new())),
        }
    }
    pub fn build<T>(&self, value: T) -> (StateGetter<T>, StateSetter<T>) {
        State::new(value, self.control.clone()).into_interface()
    }
}

impl Future for StateBuilder {
    type Output = ();

    // maybe return some error if no `StateSetter` exists?
    fn poll(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<()> {
        let control_read_upgradable = self.control.upgradable_read();
        let new_waker = ctx.waker();
        if control_read_upgradable.waker.will_wake(new_waker) {
            if control_read_upgradable
                .pending_changes
                .load(atomic::Ordering::Relaxed)
            {
                let mut control_write = RwLockUpgradableReadGuard::upgrade(control_read_upgradable);
                control_write.revision += 1;
                control_write
                    .pending_changes
                    .store(false, atomic::Ordering::Relaxed);
                Poll::Ready(())
            } else {
                // this branch returns without any write locks
                Poll::Pending
            }
        } else {
            let mut control_write = RwLockUpgradableReadGuard::upgrade(control_read_upgradable);
            control_write.waker = ctx.waker().clone();

            if control_write
                .pending_changes
                .load(atomic::Ordering::Relaxed)
            {
                control_write.revision += 1;
                control_write
                    .pending_changes
                    .store(false, atomic::Ordering::Relaxed);
                Poll::Ready(())
            } else {
                Poll::Pending
            }
        }
    }
}

struct RevisionControl {
    revision: u64,
    waker: Waker,
    pending_changes: AtomicBool,
}

impl RevisionControl {
    fn new() -> Self {
        RevisionControl {
            revision: 0,
            waker: noop_waker(),
            pending_changes: AtomicBool::new(false),
        }
    }
}

struct State<T> {
    current: T,
    pending: Option<(u64, T)>,
    control: Arc<RwLock<RevisionControl>>,
}

impl<T> State<T> {
    fn new(value: T, control: Arc<RwLock<RevisionControl>>) -> Self {
        State {
            current: value,
            pending: None,
            control,
        }
    }

    fn into_interface(self) -> (StateGetter<T>, StateSetter<T>) {
        let arcself = Arc::new(Mutex::new(self));
        (
            StateGetter {
                inner: arcself.clone(),
            },
            StateSetter { inner: arcself },
        )
    }

    fn lastest(&self) -> &T {
        self.pending
            .as_ref()
            .map(|(_r, ref val)| val)
            .unwrap_or(&self.current)
    }

    fn update(&mut self, new_val: T) {
        let control_read = self.control.read();
        let new_rev = control_read.revision;

        // Replace current value with pending one if it is from the past revision
        match self.pending.replace((new_rev, new_val)) {
            Some((old_rev, old_val)) if old_rev < new_rev => self.current = old_val,
            _ => (),
        }

        control_read
            .pending_changes
            .store(true, atomic::Ordering::Relaxed);
        control_read.waker.wake_by_ref();
    }
}

pub struct StateGetter<T> {
    inner: Arc<Mutex<State<T>>>,
}

impl<T> StateGetter<T> {
    pub fn get<'a>(&'a self) -> MappedMutexGuard<'a, T> {
        let mut state_lock = self.inner.lock();
        // Replace current commit with pending commit if it is from the past revision
        match state_lock.pending.take() {
            Some((rev, val)) if rev < state_lock.control.read().revision => {
                state_lock.current = val
            }
            still_pending => state_lock.pending = still_pending,
        }
        MutexGuard::map(state_lock, |state| &mut state.current)
    }
}

#[derive(Clone)]
pub struct StateSetter<T> {
    inner: Arc<Mutex<State<T>>>,
}

impl<T> StateSetter<T> {
    pub fn update(&self, f: impl FnOnce(&T) -> Option<T>) {
        let mut state_lock = self.inner.lock();
        if let Some(new_val) = f(state_lock.lastest()) {
            state_lock.update(new_val)
        }
    }
}
