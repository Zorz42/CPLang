import glob
import io
import os
import subprocess
import sys
import zipfile

import requests

LOG_FILE = "run.log"
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


def main():
    init_logger()

    # Make sure target/debug/cplang is built
    cplang_path = os.path.abspath("../target/debug/cplang")
    if not os.path.exists(cplang_path):
        log(f"Error: Compiler binary not found at '{cplang_path}'. Please run 'cargo build' in the workspace root.",
            to_console=True)
        sys.exit(1)

    solutions = glob.glob("solutions/*.cpl")
    if not solutions:
        log("No solutions found in solutions/ directory.", to_console=True)
        return

    log(f"Starting test run for {len(solutions)} solution(s)...")

    for sol in solutions:
        filename = os.path.basename(sol)
        # ID is the first element when split by underscore
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
        res = subprocess.run([cplang_path, sol, "-o", c_file], capture_output=True, text=True)
        if res.returncode != 0:
            log("CPLang Compilation failed:")
            log(res.stderr)
            log(res.stdout)
            log(f"[{problem_id}] {problem_name}: FAILED (CPLang compilation failed, see build/run.log)",
                to_console=True)
            continue

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

        # Find tests
        in_files = sorted(glob.glob(os.path.join(test_dir, "*.in")),
                          key=lambda x: int(os.path.basename(x).split('.')[0]))
        if not in_files:
            log(f"[{problem_id}] {problem_name}: FAILED (No test cases found in '{test_dir}')", to_console=True)
            continue

        passed_count = 0
        total_count = len(in_files)

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

            # Run binary
            run_res = subprocess.run([f"./{exe_file}"], input=input_data, capture_output=True, text=True)
            if run_res.returncode != 0:
                log(f"Test {test_name}: FAILED (Runtime Error, exit code {run_res.returncode})")
                if run_res.stderr:
                    log(f"Stderr: {run_res.stderr}")
                continue

            actual_output = run_res.stdout.split()

            if expected_output == actual_output:
                log(f"Test {test_name}: PASSED")
                passed_count += 1
            else:
                log(f"Test {test_name}: FAILED (Wrong Answer)")
                if len(expected_output) < 30 and len(actual_output) < 30:
                    log(f"  Expected: {' '.join(expected_output)}")
                    log(f"  Actual:   {' '.join(actual_output)}")

        log(f"Result: {passed_count}/{total_count} tests passed.")
        if passed_count == total_count:
            log(f"[{problem_id}] {problem_name}: PASSED ({total_count}/{total_count} tests)", to_console=True)
        else:
            log(f"[{problem_id}] {problem_name}: FAILED ({passed_count}/{total_count} tests passed, see build/run.log)",
                to_console=True)

    if _log_handle:
        _log_handle.close()


if __name__ == "__main__":
    main()
