#!/usr/bin/env python3
"""Compile the promoted clojure-test-suite runtime smoke batch."""

from __future__ import annotations

import argparse
import pathlib
import subprocess
import tempfile

import scan_clojure_suite


def read_manifest(
    path: pathlib.Path, target: str | None = None
) -> list[str]:
    namespaces = []
    seen = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        declaration = raw_line.split("#", 1)[0].strip()
        if not declaration:
            continue
        fields = declaration.split()
        if len(fields) > 2:
            raise ValueError(f"invalid promotion declaration: {declaration}")
        namespace = fields[0]
        declared_target = fields[1] if len(fields) == 2 else None
        if declared_target not in (None, "native", "melange"):
            raise ValueError(f"invalid promotion target: {declared_target}")
        if namespace in seen:
            raise ValueError(f"duplicate promoted namespace: {namespace}")
        if not namespace.startswith(scan_clojure_suite.SUITE_NAMESPACE_PREFIX):
            raise ValueError(f"not a clojure.core-test namespace: {namespace}")
        seen.add(namespace)
        if target is None or declared_target is None or declared_target == target:
            namespaces.append(namespace)
    return namespaces


def promoted_source_files(
    suite_dir: pathlib.Path, namespaces: list[str]
) -> list[pathlib.Path]:
    files = []
    seen = set()

    def append(path: pathlib.Path) -> None:
        resolved = path.resolve()
        if resolved not in seen:
            seen.add(resolved)
            files.append(path)

    for namespace in namespaces:
        test_file = scan_clojure_suite.suite_namespace_file(suite_dir, namespace)
        if not test_file.exists():
            raise FileNotFoundError(f"missing promoted suite source: {test_file}")
        for dependency in scan_clojure_suite.suite_dependency_files(
            suite_dir, test_file
        ):
            append(dependency)
        append(test_file)
    return files


def runner_source(namespaces: list[str]) -> str:
    requires = []
    aliases = set()
    for namespace in namespaces:
        suffix = namespace.removeprefix(scan_clojure_suite.SUITE_NAMESPACE_PREFIX)
        alias = suffix.replace(".", "-") + "-test"
        if alias in aliases:
            raise ValueError(f"duplicate promoted namespace alias: {alias}")
        aliases.add(alias)
        requires.append(f"   [{namespace} :as {alias}]")
    requires.append("   [clojure.test :refer [run-tests]]")
    return (
        "(ns clojure-suite-smoke\n"
        "  (:require\n"
        + "\n".join(requires)
        + "))\n\n"
        "(run-tests)\n"
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--lg-cli", type=pathlib.Path, required=True)
    parser.add_argument("--target", choices=["native", "melange"], required=True)
    parser.add_argument("--state", type=pathlib.Path, required=True)
    parser.add_argument("--test-runtime", type=pathlib.Path, required=True)
    parser.add_argument("--platform-test", type=pathlib.Path, required=True)
    parser.add_argument("--portability", type=pathlib.Path, required=True)
    parser.add_argument("--suite-dir", type=pathlib.Path, required=True)
    parser.add_argument("--manifest", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    parser.add_argument("--open-module", required=True)
    parser.add_argument(
        "--working-directory", type=pathlib.Path, default=pathlib.Path.cwd()
    )
    args = parser.parse_args()

    namespaces = read_manifest(args.manifest, target=args.target)
    sources = promoted_source_files(args.suite_dir, namespaces)
    working_directory = args.working_directory.resolve()
    with tempfile.TemporaryDirectory(prefix="lg-clojure-smoke-") as tmp:
        runner = pathlib.Path(tmp) / "runner.cljc"
        compiled_output = pathlib.Path(tmp) / "compiled.ml"
        runner.write_text(runner_source(namespaces), encoding="utf-8")
        command = [
            str(args.lg_cli.resolve()),
            "--target",
            args.target,
            "--compile-files-from",
            str(args.state.resolve()),
            str(args.test_runtime.resolve()),
            str(args.platform_test.resolve()),
            str(args.portability.resolve()),
            *(str(source.resolve()) for source in sources),
            str(runner),
            "-o",
            str(compiled_output),
        ]
        completed = subprocess.run(command, check=False, cwd=working_directory)
        if completed.returncode == 0:
            args.output.write_text(
                f"open {args.open_module}\n"
                + compiled_output.read_text(encoding="utf-8"),
                encoding="utf-8",
            )
    return completed.returncode


if __name__ == "__main__":
    raise SystemExit(main())
