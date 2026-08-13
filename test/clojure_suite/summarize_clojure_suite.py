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
        or re.search(r"unknown symbol Test[A-Za-z0-9]*(?:Record|Type)", message)
    ):
        return "static-typing-or-closed-domain-boundary"

    if "defmulti currently supports" in lower:
        return "missing-core-api-macro-or-var"

    if "unknown function letfn" in lower:
        return "unsupported-form-or-arity"

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
        or "tests called with incompatible arguments" in lower
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
        and (" and nil" in lower or "istack/-peek" in lower)
    ):
        return "negative-runtime-test-is-static-error"

    if (
        "no protocol implementation" in lower
        or "requires a generated comparator" in lower
        or "sets require" in lower
        or "requires a statically typed" in lower
        or "nullable updater" in lower
        or "update expects a function" in lower
        or "guard narrowing requires" in lower
    ):
        return "typed-protocol-or-capability-gap"

    if (
        "deftype expects" in lower
        or "cannot infer" in lower
        or "requires an explicit option element type" in lower
        or "match pattern type must match target" in lower
        or re.search(r"unknown symbol Test[A-Za-z0-9]*(?:Record|Type)", message)
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


def classify_result(result: Result) -> str:
    """Classify failures whose meaning depends on the suite namespace."""

    if (
        result.namespace
        in {
            "clojure.core-test.zero-qmark",
            "clojure.core-test.pos-qmark",
            "clojure.core-test.neg-qmark",
        }
        and "expected int arguments for " in result.error
    ):
        return "static-typing-or-closed-domain-boundary"
    if (
        result.namespace in {"clojure.core-test.max", "clojure.core-test.min"}
        and "expected int arguments for " in result.error
    ):
        return "static-typing-or-closed-domain-boundary"
    if (
        result.namespace == "clojure.core-test.realized-qmark"
        and "unknown function future" in result.error
    ):
        return "host-boundary-or-platform-specific"
    if (
        result.namespace == "clojure.core-test.with-precision"
        and "unknown function with-precision" in result.error
    ):
        return "reader-or-numeric-literal"
    return classify(result.error)


def classify_result_static_boundary(result: Result) -> str:
    """Classify static failures that require suite-file context."""

    subclass = classify_static_boundary(result.error)
    suite_fixture_errors = {
        "clojure.core-test.juxt": "juxt functions must accept the same argument type",
        "clojure.core-test.portability": "int? guard narrowing requires a statically typed value",
        "clojure.core-test.transient": "cannot infer :x as printable<inference-variable> because it is already int",
        "clojure.core-test.zero-qmark": "expected int arguments for zero?",
        "clojure.core-test.pos-qmark": "expected int arguments for pos?",
        "clojure.core-test.neg-qmark": "expected int arguments for neg?",
    }
    expected_error = suite_fixture_errors.get(result.namespace)
    if expected_error is not None and expected_error in result.error:
        return "suite-polymorphic-fixture-is-static-error"
    if (
        result.namespace in {"clojure.core-test.max", "clojure.core-test.min"}
        and "expected int arguments for " in result.error
    ):
        return "typed-protocol-or-capability-gap"
    return subclass


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


def static_error_lane(results: Iterable[Result]) -> list[dict[str, str]]:
    """Return failures that are expected static errors for negative suite tests."""

    lane = []
    for result in results:
        if result.status == "compiled":
            continue
        if classify_result(result) != "static-typing-or-closed-domain-boundary":
            continue
        static_subclass = classify_result_static_boundary(result)
        if static_subclass not in {
            "negative-runtime-test-is-static-error",
            "suite-polymorphic-fixture-is-static-error",
        }:
            continue
        lane.append(
            {
                "namespace": result.namespace,
                "target": result.target,
                "static_subclass": static_subclass,
                "error": normalize_error(result.error),
            }
        )
    return sorted(
        lane,
        key=lambda entry: (entry["namespace"], entry["target"], entry["error"]),
    )


def repair_lane_for(failure_class: str, static_subclass: str | None) -> str:
    if static_subclass in {
        "negative-runtime-test-is-static-error",
        "suite-polymorphic-fixture-is-static-error",
    }:
        return "audit-as-static-error"
    if static_subclass in {
        "dynamic-boundary-needs-closed-domain",
        "heterogeneous-collection-needs-closed-domain",
    }:
        return "design-closed-domain-or-narrow-runtime-boundary"
    if static_subclass in {
        "first-class-polymorphic-or-hof",
        "typed-protocol-or-capability-gap",
        "transient-collection-boundary",
        "form-or-declaration-static-gap",
    }:
        return "implement-static-language-capability"
    if failure_class == "reader-or-numeric-literal":
        return "design-reader-and-numeric-tower"
    if failure_class == "host-boundary-or-platform-specific":
        return "document-or-gate-host-boundary"
    if failure_class == "missing-core-api-macro-or-var":
        return "port-source-api-or-add-primitive-boundary"
    if failure_class == "missing-suite-support-namespace-or-helper":
        return "repair-suite-compatibility-scaffold"
    if failure_class in {
        "unsupported-form-or-arity",
        "unsupported-namespace-form",
        "reader-conditional-support",
        "suite-require-form-not-accepted",
    }:
        return "implement-form-or-reader-support"
    return "inspect-unclassified"


def repair_lanes(results: Iterable[Result]) -> list[dict[str, str | None]]:
    """Return one machine-readable repair lane entry per compile failure."""

    lanes = []
    for result in results:
        if result.status == "compiled":
            continue
        failure_class = classify_result(result)
        static_subclass = (
            classify_result_static_boundary(result)
            if failure_class == "static-typing-or-closed-domain-boundary"
            else None
        )
        lanes.append(
            {
                "namespace": result.namespace,
                "target": result.target,
                "class": failure_class,
                "lane": repair_lane_for(failure_class, static_subclass),
                "static_subclass": static_subclass,
                "error": normalize_error(result.error),
            }
        )
    return sorted(
        lanes,
        key=lambda entry: (entry["namespace"], entry["target"], entry["error"] or ""),
    )


def write_static_error_report(results: Iterable[Result], path: pathlib.Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(static_error_lane(results), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


def write_repair_lane_report(results: Iterable[Result], path: pathlib.Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        json.dumps(repair_lanes(results), indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )


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
    class_counts = collections.Counter(classify_result(result) for result in failures)
    static_subclass_counts = collections.Counter(
        classify_result_static_boundary(result)
        for result in failures
        if classify_result(result) == "static-typing-or-closed-domain-boundary"
    )
    repair_lane_counts = collections.Counter(
        entry["lane"] for entry in repair_lanes(failures)
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
    print("## Repair lanes")
    print()
    print("| lane | failures |")
    print("| --- | ---: |")
    for lane, count in repair_lane_counts.most_common():
        print(f"| `{lane}` | {count} |")
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
    parser.add_argument(
        "--static-error-report",
        type=pathlib.Path,
        help="write normalized negative-runtime static errors as JSON",
    )
    parser.add_argument(
        "--repair-lane-report",
        type=pathlib.Path,
        help="write normalized repair lane entries as JSON",
    )
    args = parser.parse_args()

    results = load_results(args.report)
    if args.static_error_report is not None:
        write_static_error_report(results, args.static_error_report)
    if args.repair_lane_report is not None:
        write_repair_lane_report(results, args.repair_lane_report)
    print_markdown(results, args.upstream_commit)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
