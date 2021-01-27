use std::sync::Arc;

use async_std::{io::stdin, sync::RwLock};
use futures::{executor::LocalPool, task::SpawnExt};
use moxie2::{nested, nested_interface, StateBuilder, StateGetter, StateSetter};

#[nested]
fn inner_root(state_builder: &mut StateBuilder) -> (String, StateSetter<String>) {
    #[call_local]
    let ref mut state: Option<(StateGetter<String>, StateSetter<String>)> = None;
    let state = state.get_or_insert_with(|| state_builder.build("World".to_string()));

    (format!("Hello {}!", state.0.get()), state.1.clone())
}

#[nested]
fn root(state_builder: &mut StateBuilder) -> (String, StateSetter<String>) {
    #[nest]
    inner_root(state_builder)
}

async fn async_main(callbacks: Arc<RwLock<Option<StateSetter<String>>>>) {
    let mut state_builder = StateBuilder::new();
    let mut root_state = nested_interface!(root new)();
    loop {
        let (msg, cb) = nested_interface!(root nest)(&mut state_builder, &mut root_state);
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
