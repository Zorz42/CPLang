use cplang::compile;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("compile 1083 missing number", |b| {
        b.iter(|| {
            compile(black_box("cses-tests/solutions/1083_missing_number.cpl"), black_box("main.c")).unwrap();
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
