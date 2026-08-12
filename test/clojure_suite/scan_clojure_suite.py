#!/usr/bin/env python3
"""Scan clojure-test-suite namespaces against the LG compiler.

The scanner intentionally starts with compile coverage. Runtime execution is
handled by promoted Dune smoke targets once a namespace compiles on both
targets.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import subprocess
import tempfile
import time
from dataclasses import asdict, dataclass


ROOT = pathlib.Path(__file__).resolve().parents[2]
DEFAULT_SUITE_DIR = ROOT / "vendor" / "clojure-test-suite" / "test" / "clojure" / "core_test"


@dataclass
class ScanResult:
    namespace: str
    file: str
    target: str
    status: str
    elapsed_ms: int
    error: str


def namespace_for(source: str) -> str:
    match = re.search(r"\(\s*ns\s+([^\s()]+)", source)
    if not match:
        raise ValueError("missing ns form")
    return match.group(1)


def summarize(output: str) -> str:
    lines = [line.strip() for line in output.splitlines() if line.strip()]
    useful = []
    for line in lines:
        if line.startswith("File "):
            useful.append(line)
        elif line.startswith("lg:"):
            useful.append(line)
        elif "Error:" in line or "Exception" in line:
            useful.append(line)
        elif line.startswith("Usage:"):
            useful.append(line)
    if not useful:
        useful = lines
    return "\n".join(useful[:8])


def runner_source(namespace: str) -> str:
    return (
        "(ns clojure-suite-scan-runner\n"
        "  (:require\n"
        f"   [{namespace} :as target]\n"
        "   [clojure.test :refer [run-tests]]))\n\n"
        "(run-tests)\n"
    )


def compile_namespace(test_file: pathlib.Path, namespace: str, target: str) -> ScanResult:
    state = ROOT / "_build" / "default" / "stdlib" / f"lg_stdlib_{target}.state"
    platform_test = ROOT / "test_runner" / "clojure" / (
        "test_native.cljc" if target == "native" else "test_melange.cljc"
    )
    start = time.monotonic()
    with tempfile.TemporaryDirectory(prefix="lg-clojure-suite-") as tmp:
        tmp_path = pathlib.Path(tmp)
        runner = tmp_path / "runner.cljc"
        output = tmp_path / f"{namespace.replace('.', '_').replace('-', '_')}_{target}.ml"
        runner.write_text(runner_source(namespace), encoding="utf-8")
        command = [
            "rtk",
            "dune",
            "exec",
            "--",
            "bin/lg_cli.exe",
            "--target",
            target,
            "--compile-files-from",
            str(state),
            str(ROOT / "test_runner" / "clojure" / "test.cljc"),
            str(platform_test),
            str(ROOT / "test" / "clojure_suite" / "lg_portability.cljc"),
            str(test_file),
            str(runner),
            "-o",
            str(output),
        ]
        completed = subprocess.run(
            command,
            cwd=ROOT,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            timeout=60,
        )
    elapsed_ms = int((time.monotonic() - start) * 1000)
    status = "compiled" if completed.returncode == 0 else "compile-failed"
    return ScanResult(
        namespace=namespace,
        file=str(test_file.relative_to(ROOT)),
        target=target,
        status=status,
        elapsed_ms=elapsed_ms,
        error="" if completed.returncode == 0 else summarize(completed.stdout),
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--suite-dir", type=pathlib.Path, default=DEFAULT_SUITE_DIR)
    parser.add_argument("--report", type=pathlib.Path, default=ROOT / "test" / "clojure_suite" / "scan_report.json")
    parser.add_argument("--target", action="append", choices=["native", "melange"])
    parser.add_argument("--limit", type=int)
    args = parser.parse_args()

    suite_dir = args.suite_dir
    if not suite_dir.exists():
        raise SystemExit(f"missing suite directory: {suite_dir}")

    targets = args.target or ["native", "melange"]
    files = sorted(suite_dir.glob("*.cljc"))
    if args.limit is not None:
        files = files[: args.limit]

    results: list[ScanResult] = []

    def write_report() -> None:
        args.report.parent.mkdir(parents=True, exist_ok=True)
        args.report.write_text(
            json.dumps([asdict(result) for result in results], indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )

    for index, test_file in enumerate(files, start=1):
        source = test_file.read_text(encoding="utf-8")
        namespace = namespace_for(source)
        for target in targets:
            result = compile_namespace(test_file, namespace, target)
            results.append(result)
            write_report()
            print(
                f"[{index}/{len(files)}] {target} {namespace} {result.status} {result.elapsed_ms}ms",
                flush=True,
            )

    write_report()
    failed = sum(1 for result in results if result.status != "compiled")
    print(f"wrote {args.report} ({failed}/{len(results)} failures)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
