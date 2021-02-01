use std::sync::Arc;

use async_std::{io::stdin, sync::RwLock};
use futures::{executor::LocalPool, task::SpawnExt};
use moxie2::{local_slots_interface, nested_slots, StateBuilder, StateGetter, StateSetter};

trait InnerRoot {
    #[nested_slots(namespace = "Self")]
    fn inner_root(state_builder: &mut StateBuilder) -> (String, StateSetter<String>);
}

struct Foo;
impl InnerRoot for Foo {
    #[nested_slots(namespace = "Self")]
    fn inner_root(state_builder: &mut StateBuilder) -> (String, StateSetter<String>) {
        let state = &mut local_slot!(None as Option<(StateGetter<String>, StateSetter<String>)>);
        let state = state.get_or_insert_with(|| state_builder.build("World".to_string()));

        (format!("Hello {}!", state.0.get()), state.1.clone())
    }
}

#[nested_slots]
fn root(state_builder: &mut StateBuilder) -> (String, StateSetter<String>) {
    nest![<Foo as InnerRoot>::inner_root(state_builder)]
}

async fn async_main(callbacks: Arc<RwLock<Option<StateSetter<String>>>>) {
    let mut state_builder = StateBuilder::new();
    let mut root_state = local_slots_interface!(init root)();
    loop {
        let (msg, cb) = local_slots_interface!(enter root)(&mut state_builder, &mut root_state);
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
