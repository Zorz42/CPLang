// Lets `#[count_calls]` use one stable absolute path in both this crate and
// the binary crate that consumes it.
extern crate self as cplang;

pub use crate::compiler::compile;
pub use call_counter_derive::{count_call, count_calls};

// `main.rs` and the `.cpl` test harness both live in the binary crate, which
// links this one rather than declaring `compiler` a second time — declaring it
// twice compiled the whole compiler once per crate and ran every unit test in
// it twice. These are the only items either of them needs from in here.
pub use crate::compiler::error::{CompilerError, FilePosition};
pub use crate::display_error::display_error;

pub mod call_counter;
mod compiler;
mod display_error;
