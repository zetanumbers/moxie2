pub(crate) mod local_slots;
pub(crate) mod nested_slots;
pub(crate) mod utils;

pub use local_slots::{local_slots, local_slots_interface, Api, Args, InterfaceReq, InterfaceTy};
pub use nested_slots::nested_slots;
pub use utils::{UniversalItemFn, UniversalItemType};
