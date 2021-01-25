#![feature(once_cell)]

use std::{lazy::SyncOnceCell, sync::Arc};

use async_std::{io::stdin, sync::RwLock};
use futures::{executor::LocalPool, task::SpawnExt};
use moxie2::{StateBuilder, StateGetter, StateSetter};

fn root(state_builder: &mut StateBuilder) -> (String, StateSetter<String>) {
    static STATE: SyncOnceCell<(StateGetter<String>, StateSetter<String>)> = SyncOnceCell::new();
    let state = STATE.get_or_init(|| state_builder.build("World".to_string()));

    (format!("Hello {}!", state.0.get()), state.1.clone())
}

async fn async_main(callbacks: Arc<RwLock<Option<StateSetter<String>>>>) {
    let mut state_builder = StateBuilder::new();
    loop {
        let (msg, cb) = root(&mut state_builder);
        println!("{}", msg);
        *callbacks.write().await = Some(cb);
        (&mut state_builder).await;
    }
}

async fn input_handler(callbacks: Arc<RwLock<Option<StateSetter<String>>>>) {
    let mut buffer = String::new();
    loop {
        stdin().read_line(&mut buffer).await.unwrap();
        let msg = buffer.trim();
        if msg == "exit" {
            return;
        }
        if let Some(ref cb) = *callbacks.read().await {
            cb.update(|_| Some(msg.to_string()));
        }
        buffer.clear();
    }
}

fn main() {
    let callbacks = Arc::new(RwLock::new(None));
    let mut pool = LocalPool::new();
    pool.spawner().spawn(async_main(callbacks.clone())).unwrap();
    pool.run_until(input_handler(callbacks));
}
