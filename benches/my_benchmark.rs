use cplang::compile;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

// Must match the binary's allocator, or the benchmark measures a build that
// does not exist.
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("compile 1083 missing number", |b| {
        b.iter(|| {
            compile(black_box("cses-tests/solutions/1083_missing_number.cpl"), black_box("main.c")).unwrap();
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
