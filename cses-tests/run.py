import io
import os
import subprocess
import zipfile
import glob
import sys
import time

import requests

LOG_FILE = "build/run.log"
_log_handle = None


def init_logger():
    global _log_handle
    os.makedirs("build", exist_ok=True)
    _log_handle = open(LOG_FILE, "w", encoding="utf-8")


def log(msg, to_console=False):
    if _log_handle:
        _log_handle.write(msg + "\n")
        _log_handle.flush()
    if to_console:
        print(msg)


def download_cses_tests(problem_id):
    target_dir = f"tests/{problem_id}"
    if os.path.exists(target_dir):
        return
    url = f"https://cses-tests.pages.dev/{problem_id}.zip"
    log(f"Downloading tests for {problem_id} from {url}...")
    response = requests.get(url)
    if response.status_code == 200:
        os.makedirs(target_dir, exist_ok=True)
        with zipfile.ZipFile(io.BytesIO(response.content)) as z:
            z.extractall(target_dir)
        log(f"Downloaded tests for {problem_id}")
    else:
        log(f"Failed to download tests for {problem_id}. Check if the ID is correct.")


def get_instruction_count(exe_path):
    try:
        res = subprocess.run(["objdump", "-d", exe_path], capture_output=True, text=True)
        if res.returncode != 0:
            return 0
        count = 0
        for line in res.stdout.splitlines():
            line = line.strip()
            parts = line.split(":", 1)
            if len(parts) == 2:
                addr, rest = parts
                addr = addr.strip()
                rest = rest.strip()
                if all(c in "0123456789abcdefABCDEF" for c in addr) and addr != "":
                    rest_parts = rest.split()
                    if rest_parts:
                        first_token = rest_parts[0]
                        if all(c in "0123456789abcdefABCDEF" for c in first_token) and len(first_token) >= 2:
                            count += 1
        return count
    except Exception as e:
        log(f"Error getting instruction count for {exe_path}: {e}")
        return 0


def main():
    init_logger()

    # Build release version of cplang
    log("Building cplang compiler in release mode...", to_console=True)
    build_res = subprocess.run(["cargo", "build", "--release"], cwd="..", capture_output=True, text=True)
    if build_res.returncode != 0:
        log("Cargo build failed:", to_console=True)
        log(build_res.stderr, to_console=True)
        sys.exit(1)

    cplang_path = os.path.abspath("../target/release/cplang")
    if not os.path.exists(cplang_path):
        log(f"Error: Compiler binary not found at '{cplang_path}'.", to_console=True)
        sys.exit(1)

    solutions = glob.glob("solutions/*.cpl")
    if not solutions:
        log("No solutions found in solutions/ directory.", to_console=True)
        return

    log(f"Starting test run for {len(solutions)} solution(s)...")

    # Global tracking lists
    all_c_non_ws = []
    all_c_lines = []
    all_compile_times = []
    all_inst_counts = []
    all_avg_exec_times = []
    all_max_exec_times = []

    for sol in solutions:
        filename = os.path.basename(sol)
        parts = filename.split('_')
        if not parts:
            continue
        problem_id = parts[0]
        problem_name = "_".join(parts[1:]).replace(".cpl", "")

        log(f"\n==================================================")
        log(f"Processing solution: {filename} (ID: {problem_id})")
        log(f"==================================================")

        # Download tests
        download_cses_tests(problem_id)

        test_dir = f"tests/{problem_id}"
        if not os.path.exists(test_dir):
            log(f"[{problem_id}] {problem_name}: FAILED (Test directory '{test_dir}' not found)", to_console=True)
            continue

        # Compile .cpl to .c
        c_file = f"build/{problem_id}.c"
        log(f"Compiling {sol} to {c_file}...")
        
        start_compile = time.perf_counter()
        res = subprocess.run([cplang_path, sol, "-o", c_file], capture_output=True, text=True)
        compile_time = time.perf_counter() - start_compile
        
        if res.returncode != 0:
            log("CPLang Compilation failed:")
            log(res.stderr)
            log(res.stdout)
            log(f"[{problem_id}] {problem_name}: FAILED (CPLang compilation failed, see build/run.log)", to_console=True)
            continue

        # Measure generated C file stats
        try:
            with open(c_file, "r", encoding="utf-8") as f:
                c_content = f.read()
            c_lines = len(c_content.splitlines())
            c_non_ws = len("".join(c_content.split()))
        except Exception as e:
            log(f"Error reading generated C file stats: {e}")
            c_lines = 0
            c_non_ws = 0

        # Compile .c to executable
        exe_file = f"build/{problem_id}"
        log(f"Compiling {c_file} to executable {exe_file}...")
        res = subprocess.run(["gcc", "-O2", c_file, "-o", exe_file], capture_output=True, text=True)
        if res.returncode != 0:
            log("GCC Compilation failed:")
            log(res.stderr)
            log(res.stdout)
            log(f"[{problem_id}] {problem_name}: FAILED (GCC compilation failed, see build/run.log)", to_console=True)
            continue

        # Instruction count
        inst_count = get_instruction_count(exe_file)

        # Find tests
        in_files = sorted(glob.glob(os.path.join(test_dir, "*.in")), key=lambda x: int(os.path.basename(x).split('.')[0]))
        if not in_files:
            log(f"[{problem_id}] {problem_name}: FAILED (No test cases found in '{test_dir}')", to_console=True)
            continue

        passed_count = 0
        total_count = len(in_files)
        exec_times = []

        for in_file in in_files:
            test_name = os.path.basename(in_file).split('.')[0]
            out_file = os.path.join(test_dir, f"{test_name}.out")
            if not os.path.exists(out_file):
                log(f"Warning: Output file '{out_file}' not found for '{in_file}'. Skipping.")
                continue

            with open(in_file, 'r') as f:
                input_data = f.read()

            with open(out_file, 'r') as f:
                expected_output = f.read().split()

            # Run 1: Warmup
            subprocess.run([f"./{exe_file}"], input=input_data, capture_output=True, text=True)

            # Run 2: Measure
            start_run = time.perf_counter()
            run_res = subprocess.run([f"./{exe_file}"], input=input_data, capture_output=True, text=True)
            run_time = time.perf_counter() - start_run

            if run_res.returncode != 0:
                log(f"Test {test_name}: FAILED (Runtime Error, exit code {run_res.returncode})")
                if run_res.stderr:
                    log(f"Stderr: {run_res.stderr}")
                continue

            actual_output = run_res.stdout.split()

            if expected_output == actual_output:
                log(f"Test {test_name}: PASSED | Time: {run_time*1000:.3f} ms")
                passed_count += 1
                exec_times.append(run_time)
            else:
                log(f"Test {test_name}: FAILED (Wrong Answer)")
                if len(expected_output) < 30 and len(actual_output) < 30:
                    log(f"  Expected: {' '.join(expected_output)}")
                    log(f"  Actual:   {' '.join(actual_output)}")

        log(f"Result: {passed_count}/{total_count} tests passed.")
        
        avg_exec = sum(exec_times) / len(exec_times) if exec_times else 0.0
        max_exec = max(exec_times) if exec_times else 0.0

        if passed_count == total_count:
            # Store stats for global summary only if problem passed
            all_c_non_ws.append(c_non_ws)
            all_c_lines.append(c_lines)
            all_compile_times.append(compile_time)
            all_inst_counts.append(inst_count)
            all_avg_exec_times.append(avg_exec)
            all_max_exec_times.append(max_exec)

            log_msg = (
                f"[{problem_id}] {problem_name}: PASSED ({total_count}/{total_count} tests) | "
                f"C Size: {c_non_ws} chars, {c_lines} lines | "
                f"Compile: {compile_time*1000:.1f}ms | "
                f"Instructions: {inst_count} | "
                f"Exec: avg {avg_exec*1000:.3f}ms, max {max_exec*1000:.3f}ms"
            )
            log(log_msg, to_console=True)
        else:
            log(f"[{problem_id}] {problem_name}: FAILED ({passed_count}/{total_count} tests passed, see build/run.log)", to_console=True)

    # Output global statistics summary
    if all_c_non_ws:
        num_problems = len(all_c_non_ws)
        log("\n" + "="*50, to_console=True)
        log(f"OVERALL AVERAGE STATISTICS (Across {num_problems} successfully compiled/passed problem(s)):", to_console=True)
        log(f"  Avg C Non-whitespace Characters : {sum(all_c_non_ws)/num_problems:.1f}", to_console=True)
        log(f"  Avg C Lines                     : {sum(all_c_lines)/num_problems:.1f}", to_console=True)
        log(f"  Avg CPLang Compile Time         : {sum(all_compile_times)/num_problems*1000:.1f} ms", to_console=True)
        log(f"  Avg Executable Instruction Count: {sum(all_inst_counts)/num_problems:.1f}", to_console=True)
        log(f"  Avg Execution Time (average)    : {sum(all_avg_exec_times)/num_problems*1000:.3f} ms", to_console=True)
        log(f"  Avg Execution Time (maximum)    : {sum(all_max_exec_times)/num_problems*1000:.3f} ms", to_console=True)
        log("="*50, to_console=True)

    if _log_handle:
        _log_handle.close()


if __name__ == "__main__":
    main()
