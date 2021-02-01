use moxie2::{local_slots_interface, nested_slots};

#[nested_slots]
fn foo() {}

// type foo_type_moxie2_macros_e67dd0c1_f2a8_4161_aa1b_18cdaec4e496 = ();
// fn foo_init_moxie2_macros_e67dd0c1_f2a8_4161_aa1b_18cdaec4e496(
// ) -> self::foo_type_moxie2_macros_e67dd0c1_f2a8_4161_aa1b_18cdaec4e496 {
//     ()
// }
// fn foo_enter_moxie2_macros_e67dd0c1_f2a8_4161_aa1b_18cdaec4e496(
//     context_moxie2_macros_e67dd0c1_f2a8_4161_aa1b_18cdaec4e496: &mut self::foo_type_moxie2_macros_e67dd0c1_f2a8_4161_aa1b_18cdaec4e496,
// ) {
// }

fn main() {
    local_slots_interface!(enter foo)(&mut local_slots_interface!(init foo)());
}
