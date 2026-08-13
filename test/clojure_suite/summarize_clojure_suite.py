#!/usr/bin/env python3
"""Summarize clojure-test-suite compile scan results.

The raw JSON report intentionally keeps one result per namespace and target.
This helper normalizes absolute paths and groups failures into repair-oriented
classes so a full scan can be audited without manually reading hundreds of
compiler diagnostics.
"""

from __future__ import annotations

import argparse
import collections
import json
import pathlib
import re
from dataclasses import dataclass
from typing import Iterable


ROOT = pathlib.Path(__file__).resolve().parents[2]
DEFAULT_REPORT = ROOT / "test" / "clojure_suite" / "scan_report.json"


@dataclass(frozen=True)
class Result:
    namespace: str
    file: str
    target: str
    status: str
    elapsed_ms: int
    error: str


def load_results(path: pathlib.Path) -> list[Result]:
    raw = json.loads(path.read_text(encoding="utf-8"))
    return [Result(**entry) for entry in raw]


def normalize_error(error: str) -> str:
    error = error.replace(str(ROOT) + "/", "")
    error = re.sub(
        r'File "[^"]*vendor/clojure-test-suite/test/clojure/core_test/([^"]+)"',
        r'File "<suite>/\1"',
        error,
    )
    error = re.sub(
        r'File "[^"]*test/clojure_suite/upstream/clojure/core_test/([^"]+)"',
        r'File "<promoted>/\1"',
        error,
    )
    error = re.sub(r"lines? \d+(?:-\d+)?, characters? [^:]+", "line <n>", error)
    error = re.sub(r"line \d+(?:, characters? [^:]+)?", "line <n>", error)
    return error


def classify(error: str) -> str:
    message = error or ""
    lower = message.lower()

    if "require entries must start with a module symbol" in lower:
        return "suite-require-form-not-accepted"

    if (
        "clojure.core-test.number-range" in message
        or "clojure.core-test.eq" in message
        or "clojure.core-test.every-qmark" in message
        or "clojure.core-test.portability" in message
        or "p/lazy-seq?" in message
    ):
        return "missing-suite-support-namespace-or-helper"

    if (
        "reader conditional feature must be a keyword" in lower
        or "splicing reader conditional must select a list or vector" in lower
    ):
        return "reader-conditional-support"

    if (
        "Java interop is not supported" in message
        or "Unbound module System" in message
        or "cljs.js" in message
        or re.search(r"\bjs/[A-Za-z]", message)
        or re.search(r"\b(?:Object|String|Long/MAX_VALUE)\b", message)
        or "unknown record type Boolean" in message
        or "unknown record type cljs.core.UUID" in message
        or "clojure.lang" in message
        or "lg namespaces do not support :import" in lower
        or "unknown function definterface" in message
        or ("remove_watch.cljc" in message and "unknown function def" in message)
    ):
        return "host-boundary-or-platform-specific"

    if (
        re.search(r"unknown symbol -?\d+(?:\.\d+)?(?:N|M)\b", message)
        or re.search(r"unknown symbol -?\d{19,}\b", message)
        or re.search(r"unknown symbol \d+/\d+", message)
        or "#uuid" in message
        or "#inst" in message
        or "unknown symbol \\" in message
    ):
        return "reader-or-numeric-literal"

    if (
        "expects " in lower
        or "type mismatch" in lower
        or "must have the same type" in lower
        or "incompatible arguments" in lower
        or "match pattern type" in lower
        or "heterogeneous " in lower
        or "cannot cross a dynamic boundary" in lower
        or "no protocol implementation" in lower
        or "sets require" in lower
        or "requires a generated comparator" in lower
        or "map literal requires an even number" in lower
        or "not supported for nil" in lower
        or "not supported for string" in lower
        or "requires a statically typed" in lower
        or "element types must match" in lower
        or "function types do not line up" in lower
        or "function type must match" in lower
        or "functions must accept the same argument type" in lower
        or "indexes must be int" in lower
        or "default must match collection element type" in lower
        or "has more fixed arguments than function parameters" in lower
        or "cannot infer" in lower
        or "requires an explicit option element type" in lower
        or "nullable updater" in lower
        or "guard narrowing requires a statically typed value" in lower
    ):
        return "static-typing-or-closed-domain-boundary"

    if "defmulti currently supports" in lower:
        return "missing-core-api-macro-or-var"

    if (
        "unknown function" in lower
        or "unknown symbol" in lower
        or "cannot refer unknown symbol" in lower
        or "unknown protocol" in lower
    ):
        return "missing-core-api-macro-or-var"

    if "unsupported" in lower or "arity" in lower or "args doesn't match" in lower:
        return "unsupported-form-or-arity"

    return "other-compiler-error"


def classify_static_boundary(error: str) -> str:
    """Classify static typing failures into action-oriented subclasses."""

    message = error or ""
    lower = message.lower()

    if (
        "eq called with incompatible arguments" in lower
        or "every-fn called with incompatible arguments" in lower
        or "function types do not line up" in lower
        or "function type must match collection elements" in lower
        or "functions must accept the same argument type" in lower
        or "has more fixed arguments than function parameters" in lower
    ):
        return "first-class-polymorphic-or-hof"

    if (
        "cannot cross a dynamic boundary" in lower
        or "records cannot cross" in lower
    ):
        return "dynamic-boundary-needs-closed-domain"

    if (
        "heterogeneous " in lower
        or "must have the same type" in lower
        or "element types must match" in lower
        or "default must match collection element type" in lower
    ):
        return "heterogeneous-collection-needs-closed-domain"

    if (
        "assoc!" in lower
        or "conj!" in lower
        or "dissoc!" in lower
        or "transient" in lower
    ):
        return "transient-collection-boundary"

    if (
        "no protocol implementation" in lower
        or "requires a generated comparator" in lower
        or "sets require" in lower
        or "requires a statically typed" in lower
        or "nullable updater" in lower
        or "guard narrowing requires" in lower
    ):
        return "typed-protocol-or-capability-gap"

    if (
        "deftype expects" in lower
        or "cannot infer" in lower
        or "requires an explicit option element type" in lower
        or "match pattern type must match target" in lower
    ):
        return "form-or-declaration-static-gap"

    if (
        "type mismatch" in lower
        or "expects " in lower
        or "incompatible arguments" in lower
        or "not supported for nil" in lower
        or "not supported for string" in lower
        or "indexes must be int" in lower
        or "map literal requires an even number" in lower
    ):
        return "negative-runtime-test-is-static-error"

    return "other-static-boundary"


def grouped_by_namespace(results: Iterable[Result]) -> dict[str, dict[str, Result]]:
    grouped: dict[str, dict[str, Result]] = collections.defaultdict(dict)
    for result in results:
        grouped[result.namespace][result.target] = result
    return dict(grouped)


def namespace_outcome(native_status: str, melange_status: str) -> str:
    if native_status == "compiled" and melange_status == "compiled":
        return "compiled-both"
    if native_status == "compiled":
        return "native-only"
    if melange_status == "compiled":
        return "melange-only"
    return "failed-both"


def print_markdown(results: list[Result], upstream_commit: str | None) -> None:
    status_counts = collections.Counter(result.status for result in results)
    target_counts: dict[str, collections.Counter[str]] = collections.defaultdict(collections.Counter)
    for result in results:
        target_counts[result.target][result.status] += 1

    by_ns = grouped_by_namespace(results)
    outcome_counts: collections.Counter[str] = collections.Counter()
    for target_results in by_ns.values():
        native_status = target_results.get("native", Result("", "", "native", "missing", 0, "")).status
        melange_status = target_results.get("melange", Result("", "", "melange", "missing", 0, "")).status
        outcome_counts[namespace_outcome(native_status, melange_status)] += 1

    failures = [result for result in results if result.status != "compiled"]
    class_counts = collections.Counter(classify(result.error) for result in failures)
    static_subclass_counts = collections.Counter(
        classify_static_boundary(result.error)
        for result in failures
        if classify(result.error) == "static-typing-or-closed-domain-boundary"
    )
    normalized_counts = collections.Counter(normalize_error(result.error) for result in failures)

    print("# clojure-test-suite scan summary")
    print()
    print("Source:")
    print()
    print("- upstream: `https://github.com/jank-lang/clojure-test-suite`")
    if upstream_commit:
        print(f"- commit: `{upstream_commit}`")
    print(f"- raw report: `{DEFAULT_REPORT.relative_to(ROOT)}`")
    print()
    print("## Totals")
    print()
    print(f"- compile attempts: {len(results)}")
    print(f"- compiled: {status_counts['compiled']}")
    print(f"- compile failed: {status_counts['compile-failed']}")
    print(f"- namespaces: {len(by_ns)}")
    print(f"- namespaces compiled on both targets: {outcome_counts['compiled-both']}")
    print(f"- namespaces failed on both targets: {outcome_counts['failed-both']}")
    print(f"- native-only compiled namespaces: {outcome_counts['native-only']}")
    print(f"- melange-only compiled namespaces: {outcome_counts['melange-only']}")
    print()
    print("## Target totals")
    print()
    print("| target | compiled | compile failed |")
    print("| --- | ---: | ---: |")
    for target in sorted(target_counts):
        counts = target_counts[target]
        print(f"| {target} | {counts['compiled']} | {counts['compile-failed']} |")
    print()
    print("## Failure classes")
    print()
    print("| class | failures |")
    print("| --- | ---: |")
    for failure_class, count in class_counts.most_common():
        print(f"| `{failure_class}` | {count} |")
    print()
    if static_subclass_counts:
        print("## Static typing subclasses")
        print()
        print("| subclass | failures |")
        print("| --- | ---: |")
        for subclass, count in static_subclass_counts.most_common():
            print(f"| `{subclass}` | {count} |")
        print()
    print("## Namespaces compiled on both native and Melange")
    print()
    for namespace in sorted(
        namespace
        for namespace, target_results in by_ns.items()
        if namespace_outcome(
            target_results.get("native", Result("", "", "native", "missing", 0, "")).status,
            target_results.get("melange", Result("", "", "melange", "missing", 0, "")).status,
        )
        == "compiled-both"
    ):
        print(f"- `{namespace}`")
    print()
    print("## Platform-skew namespaces")
    print()
    for namespace, target_results in sorted(by_ns.items()):
        native = target_results.get("native", Result(namespace, "", "native", "missing", 0, ""))
        melange = target_results.get("melange", Result(namespace, "", "melange", "missing", 0, ""))
        outcome = namespace_outcome(native.status, melange.status)
        if outcome in {"native-only", "melange-only"}:
            failed = native if native.status != "compiled" else melange
            last_line = normalize_error(failed.error).splitlines()[-1]
            print(f"- `{namespace}`: {outcome}; {failed.target} failed with `{last_line}`")
    print()
    print("## Top normalized errors")
    print()
    for message, count in normalized_counts.most_common(25):
        last_line = message.splitlines()[-1] if message else ""
        print(f"- {count} × `{last_line}`")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--report", type=pathlib.Path, default=DEFAULT_REPORT)
    parser.add_argument("--upstream-commit")
    args = parser.parse_args()

    results = load_results(args.report)
    print_markdown(results, args.upstream_commit)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
