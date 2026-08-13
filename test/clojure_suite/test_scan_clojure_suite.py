#!/usr/bin/env python3
"""Tests for clojure-test-suite scanner helpers."""

from __future__ import annotations

import importlib.util
import pathlib
import sys
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[2]
SCANNER_PATH = ROOT / "test" / "clojure_suite" / "scan_clojure_suite.py"
SUMMARY_PATH = ROOT / "test" / "clojure_suite" / "summarize_clojure_suite.py"


def load_scanner():
    spec = importlib.util.spec_from_file_location("scan_clojure_suite", SCANNER_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load scan_clojure_suite.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def load_summary():
    spec = importlib.util.spec_from_file_location("summarize_clojure_suite", SUMMARY_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load summarize_clojure_suite.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class SummaryClassificationTests(unittest.TestCase):
    def setUp(self) -> None:
        self.summary = load_summary()

    def test_classifies_remaining_known_errors(self) -> None:
        examples = {
            "File \"<suite>/atom.cljc\", line <n>: lg: atom nil requires an explicit option element type, for example ^:ref<option<int>>":
                "static-typing-or-closed-domain-boundary",
            "File \"<suite>/atom.cljc\", line <n>: lg: unknown protocol cljs.core/IAtom":
                "missing-core-api-macro-or-var",
            "File \"<suite>/boolean_qmark.cljc\", line <n>: lg: unknown record type Boolean":
                "host-boundary-or-platform-specific",
            "File \"<suite>/parse_uuid.cljc\", line <n>: lg: unknown record type cljs.core.UUID":
                "host-boundary-or-platform-specific",
            "File \"<suite>/num.cljc\", line <n>: lg: unknown function definterface":
                "host-boundary-or-platform-specific",
            "File \"<suite>/remove_watch.cljc\", line <n>: lg: unknown function def":
                "host-boundary-or-platform-specific",
            "File \"<suite>/reduce.cljc\", line <n>: lg: lg namespaces do not support :import":
                "host-boundary-or-platform-specific",
        }

        for message, expected in examples.items():
            with self.subTest(message=message):
                self.assertEqual(expected, self.summary.classify(message))

    def test_classifies_reader_and_numeric_literals_before_missing_vars(self) -> None:
        examples = [
            "File \"<suite>/abs.cljc\", line <n>: lg: unknown symbol 123.456M",
            "File \"<suite>/decimal_qmark.cljc\", line <n>: lg: unknown symbol 0.0M",
            "File \"<suite>/identity.cljc\", line <n>: lg: unknown symbol #inst",
            "File \"<suite>/long.cljc\", line <n>: lg: unknown symbol -9223372036854775808",
        ]

        for message in examples:
            with self.subTest(message=message):
                self.assertEqual(
                    "reader-or-numeric-literal",
                    self.summary.classify(message),
                )

    def test_classifies_static_boundary_details(self) -> None:
        self.assertTrue(
            hasattr(self.summary, "classify_static_boundary"),
            "summarizer should expose static boundary subclasses",
        )

        examples = {
            "File \"<suite>/eq.cljc\", line <n>: lg: eq called with incompatible arguments: expected (nil, nil), got (bool, bool)":
                "first-class-polymorphic-or-hof",
            "File \"<suite>/some.cljc\", line <n>: lg: some function type must match collection elements":
                "first-class-polymorphic-or-hof",
            "File \"<suite>/add_watch.cljc\", line <n>: lg: records cannot cross a dynamic boundary; define a closed sum type containing the supported records":
                "dynamic-boundary-needs-closed-domain",
            "File \"<suite>/butlast.cljc\", line <n>: lg: heterogeneous vector has element types int | keyword; define a sum type containing these types":
                "heterogeneous-collection-needs-closed-domain",
            "File \"<suite>/apply.cljc\", line <n>: lg: apply argument type mismatch: expected int, got char":
                "negative-runtime-test-is-static-error",
            "File \"<suite>/drop_last.cljc\", line <n>: lg: no protocol implementation for IPending/-realized? and seq<int>":
                "typed-protocol-or-capability-gap",
            "File \"<suite>/assoc_bang.cljc\", line <n>: lg: assoc! expects a transient collection followed by key/value pairs":
                "transient-collection-boundary",
            "File \"<suite>/case.cljc\", line <n>: lg: match pattern type must match target":
                "form-or-declaration-static-gap",
        }

        for message, expected in examples.items():
            with self.subTest(message=message):
                self.assertEqual(expected, self.summary.classify_static_boundary(message))


class ScannerDependencyTests(unittest.TestCase):
    def setUp(self) -> None:
        self.scanner = load_scanner()
        self.tmp = tempfile.TemporaryDirectory(prefix="lg-clojure-suite-test-", dir=ROOT / "_build")
        self.suite_dir = pathlib.Path(self.tmp.name)

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def write_suite_file(self, name: str, source: str) -> pathlib.Path:
        path = self.suite_dir / name
        path.write_text(source, encoding="utf-8")
        return path

    def test_suite_namespace_file_maps_hyphenated_names_to_upstream_filenames(self) -> None:
        path = self.scanner.suite_namespace_file(
            self.suite_dir,
            "clojure.core-test.every-qmark",
        )

        self.assertEqual(self.suite_dir / "every_qmark.cljc", path)

    def test_suite_dependency_files_returns_recursive_dependencies_before_target(self) -> None:
        number_range = self.write_suite_file(
            "number_range.cljc",
            "(ns clojure.core-test.number-range)\n",
        )
        eq = self.write_suite_file(
            "eq.cljc",
            "(ns clojure.core-test.eq\n"
            "  (:require [clojure.core-test.number-range :as r]))\n",
        )
        not_eq = self.write_suite_file(
            "not_eq.cljc",
            "(ns clojure.core-test.not-eq\n"
            "  (:require\n"
            "   [clojure.core-test.eq :as eq]\n"
            "   [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists]]\n"
            "   [clojure.test :refer [deftest is testing]]))\n",
        )

        files = self.scanner.suite_dependency_files(self.suite_dir, not_eq)

        self.assertEqual([number_range, eq], files)

    def test_lg_portability_exposes_remaining_suite_helpers(self) -> None:
        test_file = self.write_suite_file(
            "portability_helper_probe.cljc",
            "(ns clojure.core-test.portability-helper-probe\n"
            "  (:require [clojure.core-test.portability :refer [big-int? sleep] :as p]))\n\n"
            "(def slept (sleep 0))\n"
            "(def big-int-check (big-int? 1))\n"
            "(def lazy-check (p/lazy-seq? (list 1 2)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.portability-helper-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_when_var_exists_skips_unsupported_vars(self) -> None:
        test_file = self.write_suite_file(
            "unsupported_var_probe.cljc",
            "(ns clojure.core-test.unsupported-var-probe\n"
            "  (:require [clojure.core-test.portability\n"
            "             #?(:cljs :refer-macros :default :refer)\n"
            "             [when-var-exists]]))\n\n"
            "(when-var-exists missing-lg-suite-var\n"
            "  (def impossible (definitely-not-a-function 1)))\n"
            "(when-var-exists +'\n"
            "  (def impossible-plus-squote (definitely-not-a-function 1)))\n"
            "(when-var-exists *'\n"
            "  (def impossible-star-squote (definitely-not-a-function 1)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.unsupported-var-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_async_macro_is_available_to_suite(self) -> None:
        test_file = self.write_suite_file(
            "async_macro_probe.cljc",
            "(ns clojure.core-test.async-macro-probe\n"
            "  (:require [clojure.test :refer [async deftest is]]))\n\n"
            "(deftest async-example\n"
            "  (async done\n"
            "    (is true)\n"
            "    (done)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.async-macro-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_when_var_exists_skips_local_defmulti_suite_bodies_only(self) -> None:
        test_file = self.write_suite_file(
            "local_defmulti_probe.cljc",
            "(ns clojure.core-test.local-defmulti-probe\n"
            "  (:require [clojure.test :refer [deftest is]]\n"
            "            [clojure.core-test.portability :refer [when-var-exists]]))\n\n"
            "(when-var-exists defmulti\n"
            "  (deftest local-defmulti-is-skipped\n"
            "    (defmulti my-multi first)\n"
            "    (defmethod my-multi :a [_command] :local)\n"
            "    (is (ifn? my-multi))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.local-defmulti-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)


if __name__ == "__main__":
    unittest.main()
