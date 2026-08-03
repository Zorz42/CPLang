//! Lightweight function-call metrics used by `#[count_calls]`.
//!
//! Each annotated function owns its own atomic counter. The counter is added
//! to this registry on its first call, so no central list of functions is
//! needed.

#[cfg(feature = "count_calls")]
use std::sync::atomic::{AtomicU64, Ordering};
#[cfg(feature = "count_calls")]
use std::sync::{Mutex, OnceLock};

#[cfg(feature = "count_calls")]
struct Entry {
    name: &'static str,
    counter: &'static AtomicU64,
}

#[cfg(feature = "count_calls")]
static ENTRIES: OnceLock<Mutex<Vec<Entry>>> = OnceLock::new();

#[cfg(feature = "count_calls")]
fn entries() -> &'static Mutex<Vec<Entry>> {
    ENTRIES.get_or_init(|| Mutex::new(Vec::new()))
}

/// Registers a function's counter once. This is called by `#[count_calls]`.
#[doc(hidden)]
#[cfg(feature = "count_calls")]
pub fn register(name: &'static str, counter: &'static AtomicU64) {
    let mut entries = entries().lock().unwrap_or_else(|poisoned| poisoned.into_inner());

    if !entries.iter().any(|entry| std::ptr::eq(entry.counter, counter)) {
        entries.push(Entry { name, counter });
    }
}

/// Prints all counted functions that were called at least once.
#[cfg(feature = "count_calls")]
pub fn print_counts() {
    fn format_with_spaces(n: u64) -> String {
        let s = n.to_string();
        let (sign, digits) = s.strip_prefix('-').map_or(("", s.as_str()), |d| ("-", d));

        let mut out = String::new();
        for (i, ch) in digits.chars().enumerate() {
            if i > 0 && (digits.len() - i) % 3 == 0 {
                out.push(' ');
            }
            out.push(ch);
        }
        format!("{sign}{out}")
    }

    let mut counts: Vec<(&str, u64)> = entries()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .iter()
        .map(|entry| (entry.name, entry.counter.load(Ordering::Relaxed)))
        .collect();

    counts.sort_unstable_by_key(|(name, _)| *name);

    println!("Function call counts:");
    for (name, count) in counts {
        println!("  {name}: {}", format_with_spaces(count));
    }
}

/// No-op unless the crate is built with `--features count_calls`.
#[cfg(not(feature = "count_calls"))]
pub fn print_counts() {}

#[cfg(all(test, feature = "count_calls"))]
mod tests {
    use super::entries;
    use crate::{count_call, count_calls};
    use std::sync::atomic::Ordering;

    struct Counter;

    impl Counter {
        #[count_calls]
        fn method(&self) {}
    }

    #[count_calls]
    fn free_function() {}

    #[test]
    fn macro_counts_free_functions_and_methods_separately() {
        Counter.method();
        free_function();
        free_function();
        for _ in 0..3 {
            count_call!("inline counter");
        }

        let registered = entries().lock().unwrap();
        assert_eq!(registered.len(), 3);
        assert!(registered.iter().any(|entry| entry.name.ends_with("::method")));
        assert!(registered
            .iter()
            .any(|entry| entry.name.ends_with("::free_function")));
        assert!(registered.iter().any(|entry| {
            entry.name == "inline counter"
                && entry.counter.load(Ordering::Relaxed) == 3
        }));
    }
}
