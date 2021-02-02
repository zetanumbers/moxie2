use moxie2::{local_slots_interface, nested_slots};

#[nested_slots]
fn foo() {
    #[nest]
    bar()
}

#[nested_slots]
fn bar() {
    #[local_slot]
    (13 as i32);
}

fn main() {
    local_slots_interface!(enter foo)(&mut local_slots_interface!(init foo)(&mut ()));
}
