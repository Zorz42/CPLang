// Lets `#[count_calls]` use one stable absolute path in both this crate and
// the binary crate that consumes it.
extern crate self as cplang;

pub use crate::compiler::compile;
pub use call_counter_derive::{count_call, count_calls};

pub mod call_counter;
mod compiler;
