#!/usr/bin/env python3
"""Verify that every compiled-both suite namespace has an audited outcome."""

from __future__ import annotations

import argparse
import collections
import csv
import pathlib


PREFIX = "clojure.core-test."
ALLOWED_EXCLUSION_CLASSES = {"static-error", "host-boundary"}


def read_namespace_list(path: pathlib.Path) -> set[str]:
    namespaces: set[str] = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        namespace = raw_line.split("#", 1)[0].strip()
        if not namespace:
            continue
        if not namespace.startswith(PREFIX):
            raise ValueError(f"invalid suite namespace: {namespace}")
        if namespace in namespaces:
            raise ValueError(f"duplicate suite namespace: {namespace}")
        namespaces.add(namespace)
    return namespaces


def read_promotions(path: pathlib.Path) -> set[tuple[str, str]]:
    namespaces: set[str] = set()
    targets: set[tuple[str, str]] = set()
    for raw_line in path.read_text(encoding="utf-8").splitlines():
        declaration = raw_line.split("#", 1)[0].strip()
        if not declaration:
            continue
        fields = declaration.split()
        if len(fields) not in {1, 2}:
            raise ValueError(f"invalid promotion declaration: {declaration}")
        namespace = fields[0]
        if not namespace.startswith(PREFIX):
            raise ValueError(f"invalid promoted namespace: {namespace}")
        if len(fields) == 2 and fields[1] not in {"native", "melange"}:
            raise ValueError(f"invalid promotion target: {fields[1]}")
        if namespace in namespaces:
            raise ValueError(f"duplicate promoted namespace: {namespace}")
        namespaces.add(namespace)
        declared_targets = {fields[1]} if len(fields) == 2 else {"native", "melange"}
        targets.update((namespace, target) for target in declared_targets)
    return targets


def read_exclusions(path: pathlib.Path) -> dict[tuple[str, str], tuple[str, str]]:
    with path.open(encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle, delimiter="\t")
        if reader.fieldnames != ["namespace", "target", "classification", "reason"]:
            raise ValueError(
                "exclusion inventory must have namespace, target, classification, reason"
            )
        exclusions: dict[tuple[str, str], tuple[str, str]] = {}
        for row in reader:
            namespace = row["namespace"].strip()
            target = row["target"].strip()
            classification = row["classification"].strip()
            reason = row["reason"].strip()
            if not namespace.startswith(PREFIX):
                raise ValueError(f"invalid excluded namespace: {namespace}")
            if target not in {"native", "melange", "both"}:
                raise ValueError(f"invalid exclusion target for {namespace}: {target}")
            if classification not in ALLOWED_EXCLUSION_CLASSES:
                raise ValueError(
                    f"invalid exclusion classification for {namespace}: {classification}"
                )
            if not reason or reason.lower() == "todo":
                raise ValueError(f"exclusion lacks concrete reason: {namespace}")
            targets = {"native", "melange"} if target == "both" else {target}
            for excluded_target in targets:
                key = (namespace, excluded_target)
                if key in exclusions:
                    raise ValueError(
                        f"duplicate excluded target: {namespace} {excluded_target}"
                    )
                exclusions[key] = (classification, reason)
    return exclusions


def check_coverage(
    compiled_path: pathlib.Path,
    promoted_path: pathlib.Path,
    exclusions_path: pathlib.Path,
) -> tuple[int, int, int, int]:
    compiled = read_namespace_list(compiled_path)
    promoted = read_promotions(promoted_path)
    exclusions = read_exclusions(exclusions_path)
    excluded = set(exclusions)
    compiled_targets = {
        (namespace, target)
        for namespace in compiled
        for target in ("native", "melange")
    }

    overlap = promoted & excluded & compiled_targets
    if overlap:
        raise ValueError(
            "namespaces cannot be both promoted and excluded: "
            + ", ".join(f"{namespace} {target}" for namespace, target in sorted(overlap))
        )
    extra_exclusions = excluded - compiled_targets
    if extra_exclusions:
        raise ValueError(
            "excluded namespace is not in compiled-both inventory: "
            + ", ".join(
                f"{namespace} {target}"
                for namespace, target in sorted(extra_exclusions)
            )
        )
    missing = compiled_targets - promoted - excluded
    if missing:
        raise ValueError(
            "unclassified compiled target: "
            + ", ".join(
                f"{namespace} {target}" for namespace, target in sorted(missing)
            )
        )

    promoted_compiled = compiled_targets & promoted
    promoted_namespaces = {namespace for namespace, _target in promoted}
    curated_outside_scan = promoted_namespaces - compiled
    return (
        len(compiled_targets),
        len(promoted_compiled),
        len(excluded),
        len(curated_outside_scan),
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--compiled", type=pathlib.Path, required=True)
    parser.add_argument("--promoted", type=pathlib.Path, required=True)
    parser.add_argument("--exclusions", type=pathlib.Path, required=True)
    args = parser.parse_args()
    compiled_targets, promoted_targets, excluded_targets, curated = check_coverage(
        args.compiled, args.promoted, args.exclusions
    )
    exclusion_counts = collections.Counter(
        classification
        for classification, _reason in read_exclusions(args.exclusions).values()
    )
    print(
        f"compiled-targets={compiled_targets} promoted={promoted_targets} "
        f"excluded={excluded_targets} "
        f"static-error={exclusion_counts['static-error']} "
        f"host-boundary={exclusion_counts['host-boundary']} "
        f"curated-outside-scan={curated}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
