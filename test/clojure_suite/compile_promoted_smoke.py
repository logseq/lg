#!/usr/bin/env python3
"""Compile the promoted clojure-test-suite runtime smoke batch.

Modes:
- ``all``: single lg invocation compiling the whole batch (original behavior).
- ``base``: compile only the test runtime prefix and emit a saved state.
- ``chunks``: compile the promoted namespaces as N round-robin slices off the
  base state in parallel, emitting ``include <base module>`` instead of the
  prefix, plus a runner module. Chunk modules append a ``__clj_smoke_ready``
  marker so the runner module can force link order by referencing them.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import pathlib
import subprocess
import sys
import tempfile

import scan_clojure_suite

CHUNK_MARKER = "__clj_smoke_ready"


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


def chunk_namespaces(namespaces: list[str], chunks: int, index: int) -> list[str]:
    return [
        namespace
        for position, namespace in enumerate(namespaces)
        if position % chunks == index
    ]


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


def ocaml_runner_source(chunks: int, module_prefix: str, run_module: str) -> str:
    references = "\n".join(
        f"let () = ignore {module_prefix}{index}.{CHUNK_MARKER}"
        for index in range(chunks)
    )
    return f'{references}\nlet () = {run_module}.run "clojure.test"\n'


def run_lg(command: list[str], working_directory: pathlib.Path) -> int:
    completed = subprocess.run(command, check=False, cwd=working_directory)
    return completed.returncode


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--mode", choices=["all", "base", "chunks"], default="all"
    )
    parser.add_argument("--lg-cli", type=pathlib.Path)
    parser.add_argument("--target", choices=["native", "melange"])
    parser.add_argument("--state", type=pathlib.Path)
    parser.add_argument("--test-runtime", type=pathlib.Path)
    parser.add_argument("--platform-test", type=pathlib.Path)
    parser.add_argument("--portability", type=pathlib.Path)
    parser.add_argument("--suite-dir", type=pathlib.Path)
    parser.add_argument("--manifest", type=pathlib.Path)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    parser.add_argument("--open-module")
    parser.add_argument("--emit-state", type=pathlib.Path)
    parser.add_argument("--chunk-state", type=pathlib.Path)
    parser.add_argument("--prefix-interface", type=pathlib.Path)
    parser.add_argument("--chunks", type=int, default=8)
    parser.add_argument("--module-prefix")
    parser.add_argument("--run-module")
    parser.add_argument("--runner-output")
    parser.add_argument(
        "--working-directory", type=pathlib.Path, default=pathlib.Path.cwd()
    )
    args = parser.parse_args()

    if args.mode == "chunks":
        if (
            args.lg_cli is None
            or args.target is None
            or args.chunk_state is None
            or args.prefix_interface is None
            or args.manifest is None
            or args.suite_dir is None
            or args.module_prefix is None
            or args.run_module is None
        ):
            parser.error(
                "--lg-cli, --target, --chunk-state, --prefix-interface, "
                "--manifest, --suite-dir, --module-prefix, and --run-module "
                "are required for chunks mode"
            )
        namespaces = read_manifest(args.manifest, target=args.target)
        working_directory = args.working_directory.resolve()
        chunk_state = (pathlib.Path.cwd() / args.chunk_state).resolve()
        prefix_interface = (
            pathlib.Path.cwd() / args.prefix_interface
        ).resolve()
        lg_cli = str(args.lg_cli.resolve())
        output_dir = pathlib.Path.cwd()

        def compile_chunk(index: int) -> int:
            selected = chunk_namespaces(namespaces, args.chunks, index)
            sources = promoted_source_files(args.suite_dir, selected)
            output = output_dir / f"{args.output}_{index}.ml"
            if not sources:
                output.write_text(
                    f"let {CHUNK_MARKER} = ()\n", encoding="utf-8"
                )
                return 0
            with tempfile.TemporaryDirectory(
                prefix="lg-clojure-smoke-"
            ) as tmp:
                compiled_output = pathlib.Path(tmp) / "compiled.ml"
                command = [
                    lg_cli,
                    "--target",
                    args.target,
                    "--compile-files-chunk-from",
                    str(chunk_state),
                    "--prefix-interface",
                    str(prefix_interface),
                    *(str(source.resolve()) for source in sources),
                    "-o",
                    str(compiled_output),
                ]
                completed = subprocess.run(
                    command,
                    check=False,
                    cwd=working_directory,
                    capture_output=True,
                    text=True,
                )
                if completed.returncode != 0:
                    sys.stderr.write(
                        f"chunk {index} failed:\n"
                        + completed.stdout
                        + completed.stderr
                    )
                    return completed.returncode
                output.write_text(
                    compiled_output.read_text(encoding="utf-8")
                    + f"\nlet {CHUNK_MARKER} = ()\n",
                    encoding="utf-8",
                )
                return 0

        with concurrent.futures.ThreadPoolExecutor(
            max_workers=args.chunks
        ) as pool:
            results = list(pool.map(compile_chunk, range(args.chunks)))
        if any(results):
            return 1
        runner_output = args.runner_output or f"{args.output}_runner.ml"
        (output_dir / runner_output).write_text(
            ocaml_runner_source(
                args.chunks,
                args.module_prefix,
                args.run_module,
            ),
            encoding="utf-8",
        )
        return 0

    for required in (
        "lg_cli",
        "target",
        "test_runtime",
        "platform_test",
        "portability",
        "open_module",
    ):
        if getattr(args, required) is None:
            parser.error(f"--{required.replace('_', '-')} is required")

    namespaces = (
        read_manifest(args.manifest, target=args.target)
        if args.manifest is not None
        else []
    )
    working_directory = args.working_directory.resolve()

    if args.mode == "base":
        if args.state is None or args.emit_state is None:
            parser.error("--state and --emit-state are required for base mode")
        emit_state = (pathlib.Path.cwd() / args.emit_state).resolve()
        output = pathlib.Path.cwd() / args.output
        with tempfile.TemporaryDirectory(prefix="lg-clojure-smoke-") as tmp:
            compiled_output = pathlib.Path(tmp) / "compiled.ml"
            command = [
                str(args.lg_cli.resolve()),
                "--target",
                args.target,
                "--compile-files-from",
                str(args.state.resolve()),
                "--emit-state",
                str(emit_state),
                str(args.test_runtime.resolve()),
                str(args.platform_test.resolve()),
                str(args.portability.resolve()),
                "-o",
                str(compiled_output),
            ]
            returncode = run_lg(command, working_directory)
            if returncode == 0:
                output.write_text(
                    compiled_output.read_text(encoding="utf-8"),
                    encoding="utf-8",
                )
        return returncode

    if args.state is None or args.suite_dir is None or args.manifest is None:
        parser.error("--state, --suite-dir, and --manifest are required")
    sources = promoted_source_files(args.suite_dir, namespaces)
    output = pathlib.Path.cwd() / args.output
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
        returncode = run_lg(command, working_directory)
        if returncode == 0:
            output.write_text(
                f"open {args.open_module}\n"
                + compiled_output.read_text(encoding="utf-8"),
                encoding="utf-8",
            )
    return returncode


if __name__ == "__main__":
    raise SystemExit(main())
