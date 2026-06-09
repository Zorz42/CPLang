import io
import os.path
import zipfile

import requests


def download_cses_tests(problem_id):
    if os.path.exists(f"tests/{problem_id}"):
        return
    url = f"https://cses-tests.pages.dev/{problem_id}.zip"
    response = requests.get(url)
    if response.status_code == 200:
        with zipfile.ZipFile(io.BytesIO(response.content)) as z:
            z.extractall(f"tests/{problem_id}")
        print(f"Downloaded tests for {problem_id}")
    else:
        print("Failed to download. Check if the ID is correct.")


# Usage
download_cses_tests("1068")
