use std::sync::Arc;

use async_std::{io::stdin, sync::RwLock};
use futures::{executor::LocalPool, task::SpawnExt};
use moxie2::{local_slots_interface, nested_slots, StateBuilder, StateGetter, StateSetter};

#[nested_slots(bounds = "AsMut<StateBuilder>")]
fn root() -> (String, StateSetter<String>) {
    let (ref getter, ref setter) = #[local_slot]
    (init_context.as_mut().build("World".to_string())
        as (StateGetter<String>, StateSetter<String>));

    (format!("Hello {}!", getter.get()), setter.clone())
}

async fn async_main(callbacks: Arc<RwLock<Option<StateSetter<String>>>>) {
    let mut state_builder = StateBuilder::new();
    let mut root_state = local_slots_interface!(init root)(&mut state_builder);
    loop {
        let (msg, cb) = local_slots_interface!(enter root)(&mut root_state);
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
