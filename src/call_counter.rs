//! Lightweight function-call metrics used by `#[count_calls]` and `count_call!`.
//!
//! Every annotated function owns a function-local atomic counter, so there is
//! no central list of instrumented functions to maintain: a counter joins the
//! registry the first time its function runs.
//!
//! On top of the plain totals, an annotation can record *where* the calls came
//! from. `#[count_calls(depth = N)]` walks the real call stack on every call
//! and keeps the `N - 1` return addresses above the annotated function, which
//! [`print_counts`] resolves to function names and prints as a tree of callers
//! underneath the function. Callers do not need an annotation of their own:
//! every function on the stack shows up. The default depth of `1` records the
//! total only.
//!
//! Each caller is printed with its share of the line above it, and names are
//! cut down to the shortest tail that still tells them apart, since a tree of
//! full module paths is unreadable.
//!
//! Walking the stack costs roughly 20ns per level, against about 4µs for a
//! `backtrace::trace`, because it only chases frame pointers and leaves the
//! expensive symbol lookup to [`print_counts`]. What that buys is paid for in
//! assumptions:
//!
//! * The target must be a Unix on `aarch64` or `x86_64`, and its frames must
//!   keep a frame pointer. That holds by ABI on `aarch64-apple-*`; elsewhere
//!   build with `RUSTFLAGS=-Cforce-frame-pointers=yes` to be sure. On other
//!   targets no callers are recorded and every call is reported as
//!   `(top level)`.
//! * Names come from debug info, so a stripped binary reports raw addresses.
//!   The `release` profile of this crate strips symbols; prefer a `dev` build
//!   or `--profile measure`.
//! * A caller that was inlined into *its* caller has no stack frame left to
//!   find, so optimized builds attribute those calls one level higher. A `dev`
//!   build reports the call stack exactly as written.
//!
//! Everything here compiles away unless the crate is built with
//! `--features count_calls`.

#[cfg(feature = "count_calls")]
pub use enabled::*;

/// No-op unless the crate is built with `--features count_calls`.
#[cfg(not(feature = "count_calls"))]
pub fn print_counts() {}

#[cfg(feature = "count_calls")]
mod enabled {
    use std::cell::RefCell;
    use std::collections::{BTreeMap, HashMap};
    use std::hash::{BuildHasherDefault, Hash, Hasher};
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::{Mutex, MutexGuard, OnceLock};

    /// Longest call path a single annotation can record, including the
    /// annotated function itself. `#[count_calls(depth = N)]` rejects anything
    /// larger at compile time.
    pub const MAX_TRACE_DEPTH: usize = 16;

    /// Return addresses one call can keep, which is everything above the
    /// annotated function itself.
    const MAX_CALLERS: usize = MAX_TRACE_DEPTH - 1;

    /// One instrumented function, method or `count_call!` site.
    struct Entry {
        name: &'static str,
        counter: &'static AtomicU64,
    }

    /// One call of `id`, together with the return addresses of the callers
    /// above it, innermost first.
    ///
    /// Paths are fixed size so that recording a call never allocates.
    #[derive(Clone, Copy)]
    struct TracePath {
        id: u32,
        len: u8,
        callers: [usize; MAX_CALLERS],
    }

    impl TracePath {
        fn callers(&self) -> &[usize] {
            &self.callers[..self.len as usize]
        }
    }

    impl PartialEq for TracePath {
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id && self.callers() == other.callers()
        }
    }

    impl Eq for TracePath {}

    impl Hash for TracePath {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.id.hash(state);
            self.callers().hash(state);
        }
    }

    /// An `FxHash`-style hasher. The default `SipHash` costs more than the rest
    /// of the bookkeeping put together on hot functions.
    #[derive(Default)]
    struct IdHasher(u64);

    impl IdHasher {
        const MULTIPLIER: u64 = 0x517c_c1b7_2722_0a95;

        fn add(&mut self, value: u64) {
            self.0 = (self.0.rotate_left(5) ^ value).wrapping_mul(Self::MULTIPLIER);
        }
    }

    impl Hasher for IdHasher {
        fn write(&mut self, bytes: &[u8]) {
            for &byte in bytes {
                self.add(u64::from(byte));
            }
        }

        fn write_u32(&mut self, value: u32) {
            self.add(u64::from(value));
        }

        fn write_usize(&mut self, value: usize) {
            self.add(value as u64);
        }

        fn finish(&self) -> u64 {
            self.0
        }
    }

    type TraceMap = HashMap<TracePath, u64, BuildHasherDefault<IdHasher>>;

    static ENTRIES: OnceLock<Mutex<Vec<Entry>>> = OnceLock::new();
    static TRACES: OnceLock<Mutex<TraceMap>> = OnceLock::new();

    thread_local! {
        static THREAD_TRACES: RefCell<ThreadTraces> = RefCell::new(ThreadTraces::default());
    }

    /// Per-thread path counts.
    ///
    /// Counting thread-locally and merging into [`TRACES`] once the thread ends
    /// keeps a call down to one hash lookup instead of a global lock.
    #[derive(Default)]
    struct ThreadTraces(TraceMap);

    impl ThreadTraces {
        /// Merges this thread's paths into the process-wide map.
        fn flush(&mut self) {
            if self.0.is_empty() {
                return;
            }

            let mut global = traces();
            for (path, count) in self.0.drain() {
                *global.entry(path).or_default() += count;
            }
        }
    }

    impl Drop for ThreadTraces {
        fn drop(&mut self) {
            self.flush();
        }
    }

    fn entries() -> MutexGuard<'static, Vec<Entry>> {
        ENTRIES
            .get_or_init(|| Mutex::new(Vec::new()))
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn traces() -> MutexGuard<'static, TraceMap> {
        TRACES
            .get_or_init(|| Mutex::new(TraceMap::default()))
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// Registers a counter and returns the id used to refer to it in call
    /// paths. Called once per instrumented site by the macros.
    #[doc(hidden)]
    pub fn register(name: &'static str, counter: &'static AtomicU64) -> u32 {
        let mut entries = entries();

        let index = entries.iter().position(|entry| std::ptr::eq(entry.counter, counter)).unwrap_or_else(|| {
            entries.push(Entry { name, counter });
            entries.len() - 1
        });

        u32::try_from(index).expect("at most u32::MAX instrumented functions")
    }

    /// The frame pointer of the frame this is compiled into, which is why it
    /// must never become a call of its own.
    #[cfg(all(unix, any(target_arch = "aarch64", target_arch = "x86_64")))]
    #[inline(always)]
    fn frame_pointer() -> usize {
        let frame_pointer: usize;

        // SAFETY: reads a callee-saved register into a local. The register is
        // only meaningful as a frame pointer if this frame keeps one, which the
        // walk below sanity-checks rather than trusts.
        unsafe {
            #[cfg(target_arch = "aarch64")]
            std::arch::asm!("mov {}, x29", out(reg) frame_pointer, options(nomem, nostack, preserves_flags));
            #[cfg(target_arch = "x86_64")]
            std::arch::asm!("mov {}, rbp", out(reg) frame_pointer, options(nomem, nostack, preserves_flags));
        }

        frame_pointer
    }

    /// Targets whose frame layout is not known here record no callers.
    #[cfg(not(all(unix, any(target_arch = "aarch64", target_arch = "x86_64"))))]
    #[inline(always)]
    const fn frame_pointer() -> usize {
        0
    }

    /// Alignment every frame record on the stack has.
    const FRAME_ALIGN: usize = if cfg!(target_arch = "aarch64") { 16 } else { size_of::<usize>() };

    /// How far above the current frame a frame pointer may still be plausible.
    /// Generous, because threads here run on stacks of a few hundred megabytes.
    const MAX_STACK_SPAN: usize = 1 << 30;

    /// Collects the return addresses of up to `wanted` frames above the frame
    /// this is compiled into, innermost first, and returns how many it found.
    ///
    /// Both callers rely on being compiled into the right frame, so this must
    /// stay `#[inline(always)]`: [`record_callers`] lands it in the annotated
    /// function, which makes address 0 that function's caller, and
    /// [`record_site`] keeps a frame of its own, which makes address 0 the
    /// function holding the `count_call!`.
    #[inline(always)]
    fn capture(frames: &mut [usize; MAX_CALLERS], wanted: usize) -> usize {
        let mut frame = frame_pointer();
        // Every frame record sits above the locals of its own frame, so nothing
        // below this address can be one.
        let floor = std::ptr::from_ref(&frame) as usize;

        let mut found = 0;
        while found < wanted {
            if frame < floor || frame - floor > MAX_STACK_SPAN || !frame.is_multiple_of(FRAME_ALIGN) {
                break;
            }

            // SAFETY: `frame` points at a frame record on the stack of this
            // thread: it is above this frame's own locals, below the top of the
            // stack, aligned, and either the register this walk started from or
            // a link read out of the previous record.
            let (next, return_address) = unsafe { (*(frame as *const usize), *((frame + size_of::<usize>()) as *const usize)) };

            if return_address == 0 {
                break;
            }
            frames[found] = return_address;
            found += 1;

            // The stack grows downwards, so a frame that does not link upwards
            // is not a frame at all.
            if next <= frame {
                break;
            }
            frame = next;
        }

        found
    }

    /// Counts one call of `id` along with the callers it was reached through.
    #[inline(never)]
    fn record(id: u32, callers: &[usize]) {
        let mut path = TracePath {
            id,
            len: callers.len() as u8,
            callers: [0; MAX_CALLERS],
        };
        path.callers[..callers.len()].copy_from_slice(callers);

        // `try_with` keeps this a no-op for calls made while the thread's
        // locals are being destroyed instead of panicking.
        let _ = THREAD_TRACES.try_with(|traces| *traces.borrow_mut().0.entry(path).or_default() += 1);
    }

    /// Records one call of an annotated function together with its callers.
    ///
    /// Compiled into the annotated function itself, so that the first caller it
    /// finds is the function that made the call.
    #[doc(hidden)]
    #[inline(always)]
    pub fn record_callers(id: u32, depth: usize) {
        let mut frames = [0; MAX_CALLERS];
        let found = capture(&mut frames, (depth - 1).min(MAX_CALLERS));
        record(id, &frames[..found]);
    }

    /// Records one execution of a `count_call!` site together with the
    /// functions it is nested in.
    ///
    /// Deliberately *not* inlined: keeping a frame of its own is what makes the
    /// function containing the `count_call!` the first caller it finds.
    #[doc(hidden)]
    #[inline(never)]
    pub fn record_site(id: u32, depth: usize) {
        let mut frames = [0; MAX_CALLERS];
        let found = capture(&mut frames, (depth - 1).min(MAX_CALLERS));
        record(id, &frames[..found]);
    }

    /// A node of the caller tree of one instrumented function.
    #[derive(Default)]
    struct TraceNode {
        /// Calls whose recorded path ended here, either because the call had no
        /// further caller or because the configured depth was reached.
        direct: u64,
        callers: BTreeMap<u32, TraceNode>,
    }

    impl TraceNode {
        fn insert(&mut self, callers: &[u32], count: u64) {
            match callers.split_first() {
                None => self.direct += count,
                Some((caller, rest)) => self.callers.entry(*caller).or_default().insert(rest, count),
            }
        }

        fn total(&self) -> u64 {
            self.direct + self.callers.values().map(Self::total).sum::<u64>()
        }
    }

    /// Trims the parts of a resolved symbol that only add width: the brackets
    /// an inherent method is reported in, and the module path of every type
    /// argument. `<a::b::Dsu<a::b::Node>>::get` becomes `a::b::Dsu<Node>::get`.
    fn shorten(symbol: &str) -> String {
        // A qualified path (`<T as Trait>::method`) needs its brackets to stay
        // readable; the brackets around an inherent impl do not.
        let symbol = match symbol.strip_prefix('<') {
            Some(rest) if !rest.contains(" as ") => rest.replacen(">::", "::", 1),
            _ => symbol.to_owned(),
        };

        let mut out = String::with_capacity(symbol.len());
        let mut depth = 0usize;
        // Where the path that is currently being written started, so that a
        // `::` inside type arguments can drop everything in front of it.
        let mut segment = 0;

        let mut chars = symbol.chars().peekable();
        while let Some(ch) = chars.next() {
            if depth > 0 && ch == ':' && chars.peek() == Some(&':') {
                chars.next();
                out.truncate(segment);
                continue;
            }

            out.push(ch);
            match ch {
                '<' => depth += 1,
                '>' => depth = depth.saturating_sub(1),
                _ => {}
            }
            if !ch.is_alphanumeric() && ch != '_' && ch != ':' {
                segment = out.len();
            }
        }

        out
    }

    /// Splits a path into its segments, ignoring the `::` inside the type
    /// arguments of `Dsu<a::b::Node>::get` and the like.
    fn segments(name: &str) -> Vec<&str> {
        let mut segments = Vec::new();
        let mut depth = 0usize;
        let mut start = 0;

        let bytes = name.as_bytes();
        for i in 0..bytes.len() {
            match bytes[i] {
                b'<' | b'(' => depth += 1,
                b'>' | b')' => depth = depth.saturating_sub(1),
                b':' if depth == 0 && i > start && bytes.get(i + 1) == Some(&b':') => {
                    segments.push(&name[start..i]);
                    start = i + 2;
                }
                _ => {}
            }
        }
        segments.push(&name[start..]);

        segments
    }

    /// The shortest tail of `name` that no other name in `all` ends with, never
    /// shorter than a type and its method.
    fn shortest_tail(name: &str, all: &[&str]) -> String {
        let own = segments(name);

        for keep in 2..own.len() {
            let tail = &own[own.len() - keep..];
            if !all.iter().any(|other| *other != name && segments(other).ends_with(tail)) {
                return tail.join("::");
            }
        }

        name.to_owned()
    }

    /// Turns return addresses into function names, so that every call site
    /// inside one function ends up under the same name in the tree.
    #[derive(Default)]
    struct Names {
        names: Vec<String>,
        by_address: HashMap<usize, u32>,
    }

    impl Names {
        fn get(&self, id: u32) -> &str {
            &self.names[id as usize]
        }

        /// Resolves one return address, reusing the name of an address that was
        /// already looked up and of a call site in an already named function.
        fn intern(&mut self, address: usize) -> u32 {
            if let Some(id) = self.by_address.get(&address) {
                return *id;
            }

            let name = Self::resolve(address);
            let id = self
                .names
                .iter()
                .position(|known| *known == name)
                .unwrap_or_else(|| {
                    self.names.push(name);
                    self.names.len() - 1
                })
                .try_into()
                .expect("at most u32::MAX distinct callers");

            self.by_address.insert(address, id);
            id
        }

        fn resolve(address: usize) -> String {
            let mut name = None;

            // A return address points just past the call, which can be the
            // first byte of the next function, and `resolve` reports the
            // innermost inlined function first.
            backtrace::resolve((address - 1) as *mut std::ffi::c_void, |symbol| {
                if name.is_none() {
                    name = symbol.name().map(|symbol| shorten(&format!("{symbol:#}")));
                }
            });

            // A stripped Mach-O binary answers every lookup with the image
            // header, which is worse than admitting the address is unknown.
            name.filter(|name| !name.ends_with("mh_execute_header"))
                .unwrap_or_else(|| format!("<unknown {address:#x}>"))
        }
    }

    fn format_with_spaces(n: u64) -> String {
        let digits = n.to_string();

        let mut out = String::with_capacity(digits.len() + digits.len() / 3);
        for (i, ch) in digits.chars().enumerate() {
            if i > 0 && (digits.len() - i).is_multiple_of(3) {
                out.push(' ');
            }
            out.push(ch);
        }
        out
    }

    /// One printed line: a count, its share of the line it hangs under, and the
    /// tree drawing that leads to a name.
    struct Row {
        count: u64,
        share: Option<f64>,
        label: String,
    }

    /// Lays out the callers of `node`, most frequent first, below a line that
    /// has `total` calls of its own.
    fn caller_rows(node: &TraceNode, total: u64, names: &Names, indent: &str, rows: &mut Vec<Row>) {
        let mut callers: Vec<(&str, &TraceNode)> = node.callers.iter().map(|(id, caller)| (names.get(*id), caller)).collect();
        callers.sort_by_key(|(name, caller)| (std::cmp::Reverse(caller.total()), *name));

        // Only meaningful next to callers: on its own the count is the node's
        // total, which the line above it already shows.
        let unattributed = (node.direct > 0 && !node.callers.is_empty()).then_some(node.direct);

        let last = callers.len().saturating_sub(usize::from(unattributed.is_none()));
        for (i, (name, caller)) in callers.into_iter().enumerate() {
            let count = caller.total();
            rows.push(Row {
                count,
                share: Some(share(count, total)),
                label: format!("{indent}{} {name}", if i == last { "└─" } else { "├─" }),
            });

            caller_rows(caller, count, names, &format!("{indent}{}  ", if i == last { " " } else { "│" }), rows);
        }

        if let Some(count) = unattributed {
            rows.push(Row {
                count,
                share: Some(share(count, total)),
                label: format!("{indent}└─ (top level)"),
            });
        }
    }

    fn share(count: u64, total: u64) -> f64 {
        if total == 0 { 0.0 } else { count as f64 * 100.0 / total as f64 }
    }

    /// Everything that was counted, ready to print.
    struct Report {
        /// Every instrumented site with its total, most frequent first.
        totals: Vec<(u32, String, u64)>,
        /// The caller tree of each site that recorded one, by site id.
        callers: BTreeMap<u32, TraceNode>,
        names: Names,
    }

    impl Report {
        /// Gathers the totals and resolves the recorded return addresses.
        fn collect() -> Self {
            // The thread that is printing may itself have counted calls.
            let _ = THREAD_TRACES.try_with(|traces| traces.borrow_mut().flush());

            let mut names = Names::default();
            let mut callers: BTreeMap<u32, TraceNode> = BTreeMap::new();
            for (path, count) in traces().iter() {
                let path_names: Vec<u32> = path.callers().iter().map(|address| names.intern(*address)).collect();
                callers.entry(path.id).or_default().insert(&path_names, *count);
            }

            let mut totals: Vec<(u32, String, u64)> = entries()
                .iter()
                .enumerate()
                .map(|(id, entry)| (id as u32, entry.name.to_owned(), entry.counter.load(Ordering::Relaxed)))
                .collect();
            totals.sort_by_key(|(_, name, count)| (std::cmp::Reverse(*count), name.clone()));

            // Full paths are unreadable once they are nested and repeated, and
            // the tail of a path is what tells two functions apart.
            let mut report = Self { totals, callers, names };
            report.abbreviate();
            report
        }

        /// Cuts every name down to the shortest tail that still tells it apart
        /// from all the others: `a::b::TypeResolver::merge_ref` prints as
        /// `TypeResolver::merge_ref` unless something else ends that way too.
        fn abbreviate(&mut self) {
            let full: Vec<&str> = self
                .totals
                .iter()
                .map(|(_, name, _)| name.as_str())
                .chain(self.names.names.iter().map(String::as_str))
                .collect();

            let abbreviated: Vec<String> = full.iter().map(|name| shortest_tail(name, &full)).collect();
            let (totals, names) = abbreviated.split_at(self.totals.len());

            for ((_, name, _), abbreviated) in self.totals.iter_mut().zip(totals) {
                name.clone_from(abbreviated);
            }
            self.names.names.clone_from_slice(names);
        }

        /// Every line to print, in order.
        fn rows(&self) -> Vec<Row> {
            let mut rows = Vec::new();
            for (id, name, count) in &self.totals {
                rows.push(Row {
                    count: *count,
                    share: None,
                    label: name.clone(),
                });

                if let Some(root) = self.callers.get(id) {
                    caller_rows(root, *count, &self.names, "", &mut rows);
                }
            }
            rows
        }
    }

    /// Prints every counted function that ran at least once, most frequent
    /// first, with the caller tree of the functions that asked for one.
    pub fn print_counts() {
        let rows = Report::collect().rows();

        // Counts share one right-aligned column so that they stay comparable
        // however deep in a tree they sit.
        let counts: Vec<String> = rows.iter().map(|row| format_with_spaces(row.count)).collect();
        let width = counts.iter().map(String::len).max().unwrap_or_default();

        println!("Function call counts:");
        let mut in_tree = false;
        for (row, count) in rows.iter().zip(&counts) {
            match row.share {
                Some(share) => {
                    in_tree = true;
                    println!("  {count:>width$} {share:>5.1}%  {}", row.label);
                }
                None => {
                    // Set a finished tree apart from the next function.
                    if std::mem::take(&mut in_tree) {
                        println!();
                    }
                    println!("  {count:>width$}         {}", row.label);
                }
            }
        }
    }

    /// Every recorded path, resolved to names, innermost first.
    #[cfg(test)]
    fn recorded_paths() -> Vec<(Vec<String>, u64)> {
        fn walk(node: &TraceNode, names: &Names, path: &mut Vec<String>, out: &mut Vec<(Vec<String>, u64)>) {
            out.push((path.clone(), node.total()));
            for (caller, node) in &node.callers {
                path.push(names.get(*caller).to_owned());
                walk(node, names, path, out);
                path.pop();
            }
        }

        let report = Report::collect();
        let mut out = Vec::new();
        for (id, name, _) in &report.totals {
            if let Some(root) = report.callers.get(id) {
                walk(root, &report.names, &mut vec![(*name).to_owned()], &mut out);
            }
        }
        out
    }

    #[cfg(test)]
    mod tests {
        use super::{entries, recorded_paths};
        use crate::{count_call, count_calls};
        use std::sync::atomic::Ordering;

        struct Counter;

        impl Counter {
            #[count_calls]
            fn method(&self) {}
        }

        #[count_calls]
        fn free_function() {}

        /// Asserts that `path` was recorded `expected` times, matching each
        /// name loosely so that the assertions survive the type and generic
        /// parameters a resolved symbol carries.
        #[track_caller]
        fn assert_path(path: &[&str], expected: u64) {
            let recorded = recorded_paths();
            let matched: Vec<&(Vec<String>, u64)> = recorded
                .iter()
                .filter(|(recorded, _)| recorded.len() == path.len() && recorded.iter().zip(path).all(|(name, want)| name.contains(want)))
                .collect();

            assert_eq!(matched.len(), 1, "expected exactly one path matching {path:?}, got {matched:?}");
            assert_eq!(matched[0].1, expected, "wrong count for {path:?}");
        }

        // Neither of these is annotated: the point is that they show up anyway.
        fn top(leaf: fn()) {
            middle(leaf);
        }

        fn middle(leaf: fn()) {
            leaf();
        }

        #[count_calls(depth = 4)]
        fn deep_leaf() {}

        #[count_calls(2)]
        fn shallow_leaf() {}

        #[count_calls(name = "renamed", depth = 2)]
        fn named_leaf() {}

        #[count_calls(3)]
        fn printed_leaf() {}

        #[count_calls]
        fn inline_host() {
            count_call!("hosted inline counter", 2);
        }

        #[test]
        fn counts_free_functions_and_methods_separately() {
            Counter.method();
            free_function();
            free_function();
            for _ in 0..3 {
                count_call!("inline counter");
            }
            count_call!();

            let registered = entries();
            // Other tests run in parallel in this same process, so the registry
            // can legitimately hold more than the entries used here.
            assert!(registered.iter().any(|entry| entry.name.ends_with("::method")));
            assert!(
                registered
                    .iter()
                    .any(|entry| entry.name.ends_with("::free_function") && entry.counter.load(Ordering::Relaxed) == 2)
            );
            assert!(
                registered
                    .iter()
                    .any(|entry| entry.name == "inline counter" && entry.counter.load(Ordering::Relaxed) == 3)
            );
        }

        #[test]
        fn records_callers_that_are_not_annotated() {
            top(deep_leaf);
            top(deep_leaf);

            assert_path(&["::deep_leaf", "::middle", "::top", "::records_callers_that_are_not_annotated"], 2);
        }

        #[test]
        fn truncates_callers_at_the_configured_depth() {
            top(shallow_leaf);

            assert_path(&["::shallow_leaf", "::middle"], 1);
        }

        #[test]
        fn records_calls_without_an_annotated_caller() {
            named_leaf();

            assert_path(&["renamed", "::records_calls_without_an_annotated_caller"], 1);
        }

        #[test]
        fn records_the_callers_of_an_inline_counter() {
            inline_host();

            assert_path(&["hosted inline counter", "::inline_host"], 1);
        }

        #[test]
        fn merges_paths_recorded_on_other_threads() {
            #[count_calls(2)]
            fn threaded_leaf() {}

            std::thread::spawn(|| top(threaded_leaf)).join().unwrap();

            assert_path(&["::threaded_leaf", "::middle"], 1);
        }

        #[test]
        fn lays_out_a_tree_with_calls_that_ran_out_of_stack() {
            let mut node = super::TraceNode::default();
            node.insert(&[0], 60);
            node.insert(&[1], 30);
            // Calls whose walk reached the outermost frame.
            node.insert(&[], 10);

            let names = super::Names {
                names: vec!["first".to_owned(), "second".to_owned()],
                ..Default::default()
            };
            let mut rows = Vec::new();
            super::caller_rows(&node, 100, &names, "", &mut rows);

            let rendered: Vec<String> = rows
                .iter()
                .map(|row| format!("{} {:.0}% {}", row.count, row.share.expect("a caller row has a share"), row.label))
                .collect();
            assert_eq!(rendered, ["60 60% ├─ first", "30 30% ├─ second", "10 10% └─ (top level)"]);
        }

        #[test]
        fn splits_paths_around_type_arguments() {
            assert_eq!(super::segments("a::b::Dsu<x::y::Node>::get"), ["a", "b", "Dsu<x::y::Node>", "get"]);
            assert_eq!(super::segments("plain label"), ["plain label"]);
        }

        #[test]
        fn abbreviates_names_down_to_what_tells_them_apart() {
            let all = ["a::b::Resolver::merge", "a::c::Resolver::merge", "a::b::Resolver::split", "label"];

            assert_eq!(super::shortest_tail("a::b::Resolver::split", &all), "Resolver::split");
            // Two `Resolver::merge` exist, so both have to say which one.
            assert_eq!(super::shortest_tail("a::b::Resolver::merge", &all), "b::Resolver::merge");
            assert_eq!(super::shortest_tail("label", &all), "label");
        }

        #[test]
        fn printing_counts_does_not_panic() {
            top(printed_leaf);
            super::print_counts();
        }
    }
}
