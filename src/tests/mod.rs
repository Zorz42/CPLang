//! End-to-end tests: every `.cpl` file in a subdirectory here is a test case.
//!
//! The case declares what it expects in a header of `//` comment lines — see
//! [`harness`] for the directives — and `generate_tests!` turns each file into
//! a `#[test]` named after its directory and file name.

#[cfg(test)]
mod harness;

#[cfg(test)]
#[allow(clippy::module_inception)]
mod tests {
    use super::harness::run_test;
    use test_derive::generate_tests;

    generate_tests!();
}
