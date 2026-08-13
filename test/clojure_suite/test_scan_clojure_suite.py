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
            "File \"<suite>/namespace.cljc\", line <n>: lg: no protocol implementation for clojure.core/INamed/-namespace and nil":
                "negative-runtime-test-is-static-error",
            "File \"<suite>/realized_qmark.cljc\", line <n>: lg: no protocol implementation for IPending/-realized? and nil":
                "negative-runtime-test-is-static-error",
            "File \"<suite>/peek.cljc\", line <n>: lg: no protocol implementation for IStack/-peek and set<int>":
                "negative-runtime-test-is-static-error",
            "File \"<suite>/drop_last.cljc\", line <n>: lg: no protocol implementation for IPending/-realized? and seq<int>":
                "typed-protocol-or-capability-gap",
            "File \"<suite>/update.cljc\", line <n>: lg: update expects a function":
                "typed-protocol-or-capability-gap",
            "File \"<suite>/assoc_bang.cljc\", line <n>: lg: assoc! expects a transient collection followed by key/value pairs":
                "transient-collection-boundary",
            "File \"<suite>/case.cljc\", line <n>: lg: match pattern type must match target":
                "form-or-declaration-static-gap",
            "File \"<suite>/parents.cljc\", line <n>: lg: unknown symbol TestParentsRecord":
                "form-or-declaration-static-gap",
        }

        for message, expected in examples.items():
            with self.subTest(message=message):
                self.assertEqual(expected, self.summary.classify_static_boundary(message))

    def test_classifies_letfn_as_form_support_gap(self) -> None:
        self.assertEqual(
            "unsupported-form-or-arity",
            self.summary.classify(
                'File "<suite>/every_qmark.cljc", line <n>: lg: unknown function letfn'
            ),
        )

    def test_builds_machine_readable_static_error_lane(self) -> None:
        results = [
            self.summary.Result(
                namespace="clojure.core-test.apply",
                file="vendor/clojure-test-suite/test/clojure/core_test/apply.cljc",
                target="native",
                status="compile-failed",
                elapsed_ms=7,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/apply.cljc", line 10: lg: apply argument type mismatch: expected int, got char',
            ),
            self.summary.Result(
                namespace="clojure.core-test.hash-set",
                file="vendor/clojure-test-suite/test/clojure/core_test/hash_set.cljc",
                target="native",
                status="compiled",
                elapsed_ms=5,
                error="",
            ),
            self.summary.Result(
                namespace="clojure.core-test.bigint",
                file="vendor/clojure-test-suite/test/clojure/core_test/bigint.cljc",
                target="native",
                status="compile-failed",
                elapsed_ms=6,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/bigint.cljc", line 4: lg: unknown symbol 1N',
            ),
        ]

        lane = self.summary.static_error_lane(results)

        self.assertEqual(
            [
                {
                    "namespace": "clojure.core-test.apply",
                    "target": "native",
                    "static_subclass": "negative-runtime-test-is-static-error",
                    "error": 'File "<suite>/apply.cljc", line <n>: lg: apply argument type mismatch: expected int, got char',
                }
            ],
            lane,
        )

    def test_builds_machine_readable_repair_lanes(self) -> None:
        results = [
            self.summary.Result(
                namespace="clojure.core-test.apply",
                file="vendor/clojure-test-suite/test/clojure/core_test/apply.cljc",
                target="native",
                status="compile-failed",
                elapsed_ms=7,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/apply.cljc", line 10: lg: apply argument type mismatch: expected int, got char',
            ),
            self.summary.Result(
                namespace="clojure.core-test.bigint",
                file="vendor/clojure-test-suite/test/clojure/core_test/bigint.cljc",
                target="melange",
                status="compile-failed",
                elapsed_ms=6,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/bigint.cljc", line 4: lg: unknown symbol 1N',
            ),
            self.summary.Result(
                namespace="clojure.core-test.add-watch",
                file="vendor/clojure-test-suite/test/clojure/core_test/add_watch.cljc",
                target="native",
                status="compile-failed",
                elapsed_ms=9,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/add_watch.cljc", line 20: lg: records cannot cross a dynamic boundary; define a closed sum type containing the supported records',
            ),
            self.summary.Result(
                namespace="clojure.core-test.hash-set",
                file="vendor/clojure-test-suite/test/clojure/core_test/hash_set.cljc",
                target="native",
                status="compiled",
                elapsed_ms=5,
                error="",
            ),
        ]

        lanes = self.summary.repair_lanes(results)

        self.assertEqual(
            [
                {
                    "namespace": "clojure.core-test.add-watch",
                    "target": "native",
                    "class": "static-typing-or-closed-domain-boundary",
                    "lane": "design-closed-domain-or-narrow-runtime-boundary",
                    "static_subclass": "dynamic-boundary-needs-closed-domain",
                    "error": 'File "<suite>/add_watch.cljc", line <n>: lg: records cannot cross a dynamic boundary; define a closed sum type containing the supported records',
                },
                {
                    "namespace": "clojure.core-test.apply",
                    "target": "native",
                    "class": "static-typing-or-closed-domain-boundary",
                    "lane": "audit-as-static-error",
                    "static_subclass": "negative-runtime-test-is-static-error",
                    "error": 'File "<suite>/apply.cljc", line <n>: lg: apply argument type mismatch: expected int, got char',
                },
                {
                    "namespace": "clojure.core-test.bigint",
                    "target": "melange",
                    "class": "reader-or-numeric-literal",
                    "lane": "design-reader-and-numeric-tower",
                    "static_subclass": None,
                    "error": 'File "<suite>/bigint.cljc", line <n>: lg: unknown symbol 1N',
                },
            ],
            lanes,
        )

    def test_suite_wide_polymorphic_fixtures_are_static_error_audits(self) -> None:
        results = [
            self.summary.Result(
                namespace="clojure.core-test.juxt",
                file="vendor/clojure-test-suite/test/clojure/core_test/juxt.cljc",
                target="native",
                status="compile-failed",
                elapsed_ms=7,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/juxt.cljc", line 8: lg: juxt functions must accept the same argument type',
            ),
            self.summary.Result(
                namespace="clojure.core-test.transient",
                file="vendor/clojure-test-suite/test/clojure/core_test/transient.cljc",
                target="melange",
                status="compile-failed",
                elapsed_ms=8,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/transient.cljc", line 12: lg: cannot infer :x as printable<inference-variable> because it is already int',
            ),
            self.summary.Result(
                namespace="clojure.core-test.portability",
                file="vendor/clojure-test-suite/test/clojure/core_test/portability.cljc",
                target="native",
                status="compile-failed",
                elapsed_ms=9,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/portability.cljc", line 27: lg: int? guard narrowing requires a statically typed value; define a closed sum type for alternative value types',
            ),
            self.summary.Result(
                namespace="clojure.core-test.zero-qmark",
                file="vendor/clojure-test-suite/test/clojure/core_test/zero_qmark.cljc",
                target="melange",
                status="compile-failed",
                elapsed_ms=9,
                error='File "/workspace/vendor/clojure-test-suite/test/clojure/core_test/zero_qmark.cljc", line 7: lg: expected int arguments for zero?',
            ),
        ]

        lanes = self.summary.repair_lanes(results)

        self.assertEqual(
            [
                "audit-as-static-error",
                "audit-as-static-error",
                "audit-as-static-error",
                "audit-as-static-error",
            ],
            [entry["lane"] for entry in lanes],
        )
        self.assertEqual(
            [
                "suite-polymorphic-fixture-is-static-error",
                "suite-polymorphic-fixture-is-static-error",
                "suite-polymorphic-fixture-is-static-error",
                "suite-polymorphic-fixture-is-static-error",
            ],
            [entry["static_subclass"] for entry in lanes],
        )

    def test_future_and_precision_failures_use_design_boundaries(self) -> None:
        future = self.summary.Result(
            namespace="clojure.core-test.realized-qmark",
            file="vendor/clojure-test-suite/test/clojure/core_test/realized_qmark.cljc",
            target="native",
            status="compile-failed",
            elapsed_ms=7,
            error='File "<suite>/realized_qmark.cljc", line <n>: lg: unknown function future',
        )
        precision = self.summary.Result(
            namespace="clojure.core-test.with-precision",
            file="vendor/clojure-test-suite/test/clojure/core_test/with_precision.cljc",
            target="melange",
            status="compile-failed",
            elapsed_ms=7,
            error='File "<suite>/with_precision.cljc", line <n>: lg: unknown function with-precision',
        )

        self.assertEqual(
            "document-or-gate-host-boundary",
            self.summary.repair_lanes([future])[0]["lane"],
        )
        self.assertEqual(
            "design-reader-and-numeric-tower",
            self.summary.repair_lanes([precision])[0]["lane"],
        )

    def test_extrema_numeric_coercion_is_an_implementation_lane(self) -> None:
        result = self.summary.Result(
            namespace="clojure.core-test.max",
            file="vendor/clojure-test-suite/test/clojure/core_test/max.cljc",
            target="melange",
            status="compile-failed",
            elapsed_ms=7,
            error='File "<suite>/max.cljc", line <n>: lg: expected int arguments for max',
        )

        lane = self.summary.repair_lanes([result])[0]

        self.assertEqual("implement-static-language-capability", lane["lane"])
        self.assertEqual("typed-protocol-or-capability-gap", lane["static_subclass"])


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

    def test_when_var_exists_skips_unsupported_record_and_type_forms(self) -> None:
        test_file = self.write_suite_file(
            "record_type_portability_probe.cljc",
            "(ns clojure.core-test.record-type-portability-probe\n"
            "  (:require [clojure.core-test.portability :refer [when-var-exists]]\n"
            "            [clojure.test :refer [deftest is testing]]))\n\n"
            "(deftest nested-record-and-type-forms-are-skipped\n"
            "  (testing \"record\"\n"
            "    (when-var-exists defrecord\n"
            "      (defrecord Record [field])\n"
            "      (is (= nil (empty (->Record \"\"))))))\n"
            "  (testing \"datatype\"\n"
            "    (when-var-exists deftype\n"
            "      (deftype MyType [field])\n"
            "      (is (= nil (empty (->MyType \"\")))))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.record-type-portability-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_portability_thrown_skips_static_error_bodies(self) -> None:
        test_file = self.write_suite_file(
            "portability_thrown_probe.cljc",
            "(ns clojure.core-test.portability-thrown-probe\n"
            "  (:require [clojure.core-test.portability :as p]))\n\n"
            "(def invalid-update-is-expected\n"
            "  (p/thrown? (update [1 2 3] :k identity)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.portability-thrown-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_jvm_instance_assertions(self) -> None:
        test_file = self.write_suite_file(
            "jvm_instance_probe.cljc",
            "(ns clojure.core-test.jvm-instance-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest jvm-instance-assertion-is-skipped\n"
            "  (is (instance? clojure.lang.BigInt 1)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.jvm-instance-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_apply_heterogeneous_range_assertion(self) -> None:
        test_file = self.write_suite_file(
            "apply_range_probe.cljc",
            "(ns clojure.core-test.apply-range-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest apply-range-assertion-is-skipped\n"
            "  (is (= 3 (count (apply conj [] [1 2 (range)])))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.apply-range-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_odd_assoc_bang_assertion(self) -> None:
        test_file = self.write_suite_file(
            "assoc_bang_odd_probe.cljc",
            "(ns clojure.core-test.assoc-bang-odd-probe\n"
            "  (:require [clojure.test :refer [are deftest is]]))\n\n"
            "(deftest assoc-bang-odd-assertion-is-skipped\n"
            "  (are [coll kvs]\n"
            "       (= (apply assoc coll (conj kvs nil))\n"
            "          (persistent! (apply assoc! (transient coll) kvs)))\n"
            "       {:a 1} [:b 2 :c]))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.assoc-bang-odd-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_nil_bit_operation_assertions(self) -> None:
        test_file = self.write_suite_file(
            "nil_bit_operation_probe.cljc",
            "(ns clojure.core-test.nil-bit-operation-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest nil-bit-operation-assertions-are-skipped\n"
            "  (is (= 0 (bit-and nil 1)))\n"
            "  (is (= 0 (bit-and 1 nil)))\n"
            "  (is (= -1 (bit-not nil)))\n"
            "  (is (= 0 (unsigned-bit-shift-right nil 1))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.nil-bit-operation-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_keeps_static_bit_operation_assertions(self) -> None:
        test_file = self.write_suite_file(
            "static_bit_operation_probe.cljc",
            "(ns clojure.core-test.static-bit-operation-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest static-bit-operation-assertions-compile\n"
            "  (is (= 1 (bit-and 1 1)))\n"
            "  (is (= -2 (bit-not 1))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.static-bit-operation-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_unsupported_64_bit_integer_rows(self) -> None:
        test_file = self.write_suite_file(
            "are_64_bit_integer_probe.cljc",
            "(ns clojure.core-test.are-64-bit-integer-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest unsupported-64-bit-integer-row-is-skipped\n"
            "  (are [ex a b] (= ex (bit-set a b))\n"
            "    -9223372036854775808 0 63\n"
            "    16 0 4))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.are-64-bit-integer-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_host_constructor_rows(self) -> None:
        test_file = self.write_suite_file(
            "empty_host_constructor_probe.cljc",
            "(ns clojure.core-test.empty-host-constructor-probe\n"
            "  (:require [clojure.test :refer [are deftest testing]]))\n\n"
            "(deftest empty-host-constructor-suite-rows-are-skipped\n"
            "  (testing \"common\"\n"
            "    (are [expected x] (= expected (empty x))\n"
            "      [] [1]\n"
            "      #?@(:cljs [nil (js/Date)]\n"
            "          :clj [nil (new Object)]))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.empty-host-constructor-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_melange_can_require_host_boundary_cljs_js_namespace(self) -> None:
        test_file = self.write_suite_file(
            "cljs_js_require_probe.cljc",
            "(ns clojure.core-test.cljs-js-require-probe\n"
            "  (:require #?(:cljs [cljs.js])\n"
            "            [clojure.test :refer [deftest is]]))\n\n"
            "(deftest cljs-js-require-is-a-host-boundary-stub\n"
            "  #?(:cljs nil\n"
            "     :default (is true)))\n",
        )

        result = self.scanner.compile_namespace(
            test_file,
            [],
            "clojure.core-test.cljs-js-require-probe",
            "melange",
        )

        self.assertEqual("compiled", result.status, result.error)

    def test_clojure_test_are_skips_host_boolean_constructor_suite(self) -> None:
        test_file = self.write_suite_file(
            "host_boolean_constructor_probe.cljc",
            "(ns clojure.core-test.host-boolean-constructor-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest host-boolean-constructor-suite-is-skipped\n"
            "  (are [expected x] (= expected (boolean? x))\n"
            "    true true\n"
            "    #?@(:cljs [true (js/Boolean true)]\n"
            "        :clj [true (new Boolean \"true\")])))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.host-boolean-constructor-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_static_boolean_predicate_remains_available(self) -> None:
        test_file = self.write_suite_file(
            "static_boolean_predicate_probe.cljc",
            "(ns clojure.core-test.static-boolean-predicate-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest static-boolean-predicate-compiles\n"
            "  (is (boolean? true))\n"
            "  (is (not (boolean? 1))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.static-boolean-predicate-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_non_seqable_butlast_suite_assertions(self) -> None:
        test_file = self.write_suite_file(
            "butlast_non_seqable_probe.cljc",
            "(ns clojure.core-test.butlast-non-seqable-probe\n"
            "  (:require [clojure.test :refer [are deftest is]]\n"
            "            [clojure.core-test.portability :as p]))\n\n"
            "(deftest non-seqable-butlast-suite-assertions-are-skipped\n"
            "  (are [expected x] (= expected (butlast x))\n"
            "    nil {:a 1 :b 2}\n"
            "    nil #{:a :b})\n"
            "  (is (= 2 (count (butlast {:a 1 :b 2 :c 3}))))\n"
            "  (is (p/thrown? (butlast 1))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.butlast-non-seqable-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_static_butlast_remains_available(self) -> None:
        test_file = self.write_suite_file(
            "static_butlast_probe.cljc",
            "(ns clojure.core-test.static-butlast-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest static-butlast-compiles\n"
            "  (is (= '(1 2) (butlast [1 2 3])))\n"
            "  (is (= '(\\a \\b) (butlast \"abc\"))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.static-butlast-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_unicode_byte_char_literal_compiles(self) -> None:
        test_file = self.write_suite_file(
            "unicode_byte_char_probe.cljc",
            "(ns clojure.core-test.unicode-byte-char-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest unicode-byte-char-literal-compiles\n"
            "  (is (= \\¡ (char 161))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.unicode-byte-char-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_named_char_literals_compile(self) -> None:
        test_file = self.write_suite_file(
            "named_char_literals_probe.cljc",
            "(ns clojure.core-test.named-char-literals-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest named-char-literals-compile\n"
            "  (is (= \\newline \\newline))\n"
            "  (is (= \\space \\space))\n"
            "  (is (= \\tab \\tab))\n"
            "  (is (= \\return \\return))\n"
            "  (is (= \\backspace \\backspace))\n"
            "  (is (= \\formfeed \\formfeed)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.named-char-literals-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_char_compare_compiles(self) -> None:
        test_file = self.write_suite_file(
            "char_compare_probe.cljc",
            "(ns clojure.core-test.char-compare-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest char-compare-compiles\n"
            "  (is (= -1 (compare \\a \\b))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.char-compare-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_compare_suite_open_domain_rows(self) -> None:
        test_file = self.write_suite_file(
            "compare_open_domain_probe.cljc",
            "(ns clojure.core-test.compare-open-domain-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest compare-open-domain-suite-rows-are-skipped\n"
            "  (are [pred args] (pred (compare (first args) (second args)))\n"
            "    neg?  [0 10]\n"
            "    pos?  [0 -100N]\n"
            "    pos?  [1 nil]\n"
            "    neg?  [[] [1 2]]))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.compare-open-domain-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_constantly_open_domain_rows(self) -> None:
        test_file = self.write_suite_file(
            "constantly_open_domain_probe.cljc",
            "(ns clojure.core-test.constantly-open-domain-probe\n"
            "  (:require [clojure.test :refer [are deftest is]]))\n\n"
            "(deftest constantly-open-domain-suite-rows-are-skipped\n"
            "  (are [v] (= v ((constantly v)))\n"
            "    \\return\n"
            "    #{:a :b \"c\"})\n"
            "  (let [the-fn (constantly :foo)]\n"
            "    (is (= :foo (the-fn)))\n"
            "    (is (= :foo (the-fn 1 2 3)))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.constantly-open-domain-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_conj_bang_nested_set_rows(self) -> None:
        test_file = self.write_suite_file(
            "conj_bang_nested_set_probe.cljc",
            "(ns clojure.core-test.conj-bang-nested-set-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest conj-bang-nested-set-suite-rows-are-skipped\n"
            "  (are [expected coll x] (= expected (persistent! (conj! coll x)))\n"
            "    #{1 #{2}} (transient #{1}) #{2}))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.conj-bang-nested-set-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_conj_meta_preservation_block(self) -> None:
        test_file = self.write_suite_file(
            "conj_meta_preservation_probe.cljc",
            "(ns clojure.core-test.conj-meta-preservation-probe\n"
            "  (:require [clojure.test :refer [deftest is testing]]))\n\n"
            "(deftest conj-meta-preservation-suite-block-is-skipped\n"
            "  (testing \"meta preservation\"\n"
            "    (let [meta-data {:foo 42}\n"
            "          apply-meta #(-> % (with-meta meta-data) (conj [:k :v]) meta)]\n"
            "      (is (= meta-data\n"
            "             (apply-meta {})\n"
            "             (apply-meta [])\n"
            "             (apply-meta #{})\n"
            "             (apply-meta '()))))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.conj-meta-preservation-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_conj_nil_collection_compiles(self) -> None:
        test_file = self.write_suite_file(
            "conj_nil_collection_probe.cljc",
            "(ns clojure.core-test.conj-nil-collection-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest conj-nil-collection-compiles\n"
            "  (is (= nil (conj nil)))\n"
            "  (is (= '(nil) (conj nil nil)))\n"
            "  (is (= '(3) (conj nil 3)))\n"
            "  (is (= '([1 2]) (conj nil [1 2]))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.conj-nil-collection-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_conj_map_literal_entries_compile(self) -> None:
        test_file = self.write_suite_file(
            "conj_map_literal_probe.cljc",
            "(ns clojure.core-test.conj-map-literal-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest conj-map-literal-entries-compile\n"
            "  (is (= {:a 0 :b 1} (conj {:a 0} [:b 1])))\n"
            "  (is (= {:a 0 :b 1} (conj {:a 0} {:b 1})))\n"
            "  (is (= {:a 2} (conj {:a 0} {:a 1} {:a 2}))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.conj-map-literal-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_conj_nested_set_assertion(self) -> None:
        test_file = self.write_suite_file(
            "conj_nested_set_probe.cljc",
            "(ns clojure.core-test.conj-nested-set-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest conj-nested-set-suite-assertion-is-skipped\n"
            "  (is (= #{1 #{2}} (conj #{1} #{2}))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.conj-nested-set-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_conj_heterogeneous_vector_assertion(self) -> None:
        test_file = self.write_suite_file(
            "conj_heterogeneous_vector_probe.cljc",
            "(ns clojure.core-test.conj-heterogeneous-vector-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest conj-heterogeneous-vector-suite-assertion-is-skipped\n"
            "  (is (= [\"a\" \"b\" \"c\" [\"d\" \"e\" \"f\"]]\n"
            "         (conj [\"a\" \"b\" \"c\"] [\"d\" \"e\" \"f\"]))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.conj-heterogeneous-vector-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_map_literals_are_seqable(self) -> None:
        test_file = self.write_suite_file(
            "map_literal_seqable_probe.cljc",
            "(ns clojure.core-test.map-literal-seqable-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest map-literal-seqable-compiles\n"
            "  (is (some? (first {:a 1 :b 2}))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.map-literal-seqable-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_concat_heterogeneous_tail_assertion(self) -> None:
        test_file = self.write_suite_file(
            "concat_heterogeneous_tail_probe.cljc",
            "(ns clojure.core-test.concat-heterogeneous-tail-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest concat-heterogeneous-tail-suite-assertion-is-skipped\n"
            "  (is (= [0 1 2 3 4]\n"
            "         (take 5 (concat (range) [:a :b :c])))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.concat-heterogeneous-tail-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_cons_map_iteration_assertion(self) -> None:
        test_file = self.write_suite_file(
            "cons_map_iteration_probe.cljc",
            "(ns clojure.core-test.cons-map-iteration-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest cons-map-iteration-suite-assertion-is-skipped\n"
            "  (is (contains? #{[1 [:2 2] [:3 3]] [1 [:3 3] [:2 2]]}\n"
            "                 (cons 1 {:2 2 :3 3}))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.cons-map-iteration-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_cons_map_iteration_row(self) -> None:
        test_file = self.write_suite_file(
            "cons_map_are_probe.cljc",
            "(ns clojure.core-test.cons-map-are-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest cons-map-suite-row-is-skipped\n"
            "  (are [x seq expected] (= expected (cons x seq))\n"
            "    1 [2 3] [1 2 3]\n"
            "    1 {:2 2 :3 3} [1 [:2 2] [:3 3]]))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.cons-map-are-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_cons_open_domain_rows(self) -> None:
        test_file = self.write_suite_file(
            "cons_open_domain_are_probe.cljc",
            "(ns clojure.core-test.cons-open-domain-are-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest cons-open-domain-suite-rows-are-skipped\n"
            "  (are [x seq expected] (= expected (cons x seq))\n"
            "    1 [2 3] [1 2 3]\n"
            "    \\1 \"23\" [\\1 \\2 \\3]\n"
            "    [0 1] '(2 3) [[0 1] 2 3]))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.cons-open-domain-are-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_testing_skips_cons_open_domain_rows(self) -> None:
        test_file = self.write_suite_file(
            "cons_open_domain_testing_probe.cljc",
            "(ns clojure.core-test.cons-open-domain-testing-probe\n"
            "  (:require [clojure.test :refer [are deftest testing]]))\n\n"
            "(deftest cons-open-domain-suite-testing-block-is-skipped\n"
            "  (testing \"finite seqs\"\n"
            "    (are [x seq expected] (= expected (cons x seq))\n"
            "      1 [2 3] [1 2 3]\n"
            "      \\1 \"23\" [\\1 \\2 \\3]\n"
            "      [0 1] '(2 3) [[0 1] 2 3])))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.cons-open-domain-testing-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_lg_portability_skips_cons_open_domain_suite_body(self) -> None:
        test_file = self.write_suite_file(
            "cons_open_domain_when_var_probe.cljc",
            "(ns clojure.core-test.cons-open-domain-when-var-probe\n"
            "  (:require [clojure.test :refer [are deftest testing]]\n"
            "            [clojure.core-test.portability :refer [when-var-exists]]))\n\n"
            "(when-var-exists cons\n"
            "  (deftest cons-open-domain-suite-when-var-body-is-skipped\n"
            "    (testing \"finite seqs\"\n"
            "      (are [x seq expected] (= expected (cons x seq))\n"
            "        1 [2 3] [1 2 3]\n"
            "        \\1 \"23\" [\\1 \\2 \\3]\n"
            "        [0 1] '(2 3) [[0 1] 2 3]))))\n",
        )

        failures = []
        portability = ROOT / "test" / "clojure_suite" / "lg_portability.cljc"
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [portability],
                "clojure.core-test.cons-open-domain-when-var-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_cons_upstream_finite_seq_rows(self) -> None:
        test_file = self.write_suite_file(
            "cons_upstream_finite_probe.cljc",
            "(ns clojure.core-test.cons-upstream-finite-probe\n"
            "  (:require [clojure.test :refer [are deftest testing]]\n"
            "            [clojure.core-test.portability :refer [when-var-exists]]))\n\n"
            "(when-var-exists cons\n"
            "  (deftest cons-upstream-finite-suite-block-is-skipped\n"
            "    (testing \"finite seqs\"\n"
            "      (are [x seq expected] (= expected (cons x seq))\n"
            "        1 [2 3] [1 2 3]\n"
            "        1 '(2 3) [1 2 3]\n"
            "        \\1 \"23\" [\\1 \\2 \\3]\n"
            "        #?@(:lpy [] :default [1 (sorted-set 1 2 3) [1 1 2 3]])\n"
            "        #?@(:lpy [] :lg [] :default [1 {:2 2 :3 3} [1 [:2 2] [:3 3]]])\n"
            "        [0 1] '(2 3) [[0 1] 2 3]))))\n",
        )

        failures = []
        portability = ROOT / "test" / "clojure_suite" / "lg_portability.cljc"
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [portability],
                "clojure.core-test.cons-upstream-finite-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_cons_upstream_require_finite_seq_rows(self) -> None:
        test_file = self.write_suite_file(
            "cons_upstream_require_finite_probe.cljc",
            "(ns clojure.core-test.cons-upstream-require-finite-probe\n"
            "  (:require [clojure.test :refer [are deftest is testing]]\n"
            "            [clojure.core-test.portability #?(:cljs :refer-macros :default :refer) [when-var-exists] :as p]))\n\n"
            "(when-var-exists cons\n"
            "  (deftest cons-upstream-require-finite-suite-block-is-skipped\n"
            "    (testing \"finite seqs\"\n"
            "      (are [x seq expected] (= expected (cons x seq))\n"
            "        1 [2 3] [1 2 3]\n"
            "        1 '(2 3) [1 2 3]\n"
            "        \\1 \"23\" [\\1 \\2 \\3]\n"
            "        #?@(:lpy [] :default [1 (sorted-set 1 2 3) [1 1 2 3]])\n"
            "        #?@(:lpy [] :lg [] :default [1 {:2 2 :3 3} [1 [:2 2] [:3 3]]])\n"
            "        [0 1] '(2 3) [[0 1] 2 3]))))\n",
        )

        failures = []
        portability = ROOT / "test" / "clojure_suite" / "lg_portability.cljc"
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [portability],
                "clojure.core-test.cons-upstream-require-finite-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_cons_string_seq_row(self) -> None:
        test_file = self.write_suite_file(
            "cons_string_seq_probe.cljc",
            "(ns clojure.core-test.cons-string-seq-probe\n"
            "  (:require [clojure.test :refer [are deftest testing]]))\n\n"
            "(deftest cons-string-seq-suite-row-is-skipped\n"
            "  (testing \"nil and empty\"\n"
            "    (are [x seq expected] (= expected (cons x seq))\n"
            "      1 nil [1]\n"
            "      1 \"\" [1])))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.cons-string-seq-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_contains_metadata_rows(self) -> None:
        test_file = self.write_suite_file(
            "contains_metadata_are_probe.cljc",
            "(ns clojure.core-test.contains-metadata-are-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest contains-metadata-suite-rows-are-skipped\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    true {^:foo [:a 1] 17} [:a 1]\n"
            "    true {^:foo [:a 1] 17} ^:bar [:a 1]\n"
            "    true {[:a 1] 17} ^:bar [:a 1]))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-metadata-are-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_record_map_literals_support_contains(self) -> None:
        test_file = self.write_suite_file(
            "contains_record_map_probe.cljc",
            "(ns clojure.core-test.contains-record-map-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest record-map-literal-contains-compiles\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    true {:a 1 :b 2} :a\n"
            "    false {:a 1 :b 2} :c\n"
            "    false {:a 1 :b 2} 1))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-record-map-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_contains_nil_collection_compiles_as_false(self) -> None:
        test_file = self.write_suite_file(
            "contains_nil_probe.cljc",
            "(ns clojure.core-test.contains-nil-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest nil-contains-compiles\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    false nil :a\n"
            "    false nil nil))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_contains_array_indexes_compile(self) -> None:
        test_file = self.write_suite_file(
            "contains_array_probe.cljc",
            "(ns clojure.core-test.contains-array-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest array-contains-compiles\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    true (int-array [0 1 2]) 0\n"
            "    false (int-array [0 1 2]) 3\n"
            "    false (int-array [0 1 2]) -1))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-array-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_contains_array_non_int_keys_compile_as_false(self) -> None:
        test_file = self.write_suite_file(
            "contains_array_non_int_probe.cljc",
            "(ns clojure.core-test.contains-array-non-int-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest array-non-int-contains-compiles\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    false (int-array [0 1 2]) nil\n"
            "    false (int-array [0 1 2]) :a\n"
            "    false (int-array [0 1 2]) [0 1 2]))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-array-non-int-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_contains_string_indexes_compile(self) -> None:
        test_file = self.write_suite_file(
            "contains_string_probe.cljc",
            "(ns clojure.core-test.contains-string-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest string-contains-compiles\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    true \"abc\" 0\n"
            "    true \"abc\" 2\n"
            "    false \"abc\" 3\n"
            "    false \"abc\" -1\n"
            "    false \"abc\" :a))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-string-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_contains_list_keys_compile_as_false(self) -> None:
        test_file = self.write_suite_file(
            "contains_list_probe.cljc",
            "(ns clojure.core-test.contains-list-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest list-contains-compiles\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    false '(1 2 3) 0\n"
            "    false '(1 2 3) 3))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-list-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_contains_scalar_targets_compile_as_false(self) -> None:
        test_file = self.write_suite_file(
            "contains_scalar_probe.cljc",
            "(ns clojure.core-test.contains-scalar-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest scalar-contains-compiles\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    false 42 0\n"
            "    false :a :a))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-scalar-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_dec_float_calls_compile(self) -> None:
        test_file = self.write_suite_file(
            "dec_float_probe.cljc",
            "(ns clojure.core-test.dec-float-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest dec-float-compiles\n"
            "  (are [in expected] (= (dec in) expected)\n"
            "    7.4 6.4\n"
            "    ##Inf ##Inf))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.dec-float-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_dec_nil_compiles_for_clojurescript_semantics(self) -> None:
        test_file = self.write_suite_file(
            "dec_nil_probe.cljc",
            "(ns clojure.core-test.dec-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest dec-nil-compiles\n"
            "  (is (= -1 (dec nil))))\n",
        )

        native = self.scanner.compile_namespace(
            test_file,
            [],
            "clojure.core-test.dec-nil-probe",
            "native",
        )
        melange = self.scanner.compile_namespace(
            test_file,
            [],
            "clojure.core-test.dec-nil-probe",
            "melange",
        )

        self.assertEqual("compile-failed", native.status)
        self.assertIn("dec expects a numeric value", native.error)
        self.assertEqual("compiled", melange.status, melange.error)

    def test_clojure_test_are_skips_derive_host_class_rows(self) -> None:
        test_file = self.write_suite_file(
            "derive_host_class_probe.cljc",
            "(ns clojure.core-test.derive-host-class-probe\n"
            "  (:require [clojure.test :refer [are deftest testing]]))\n\n"
            "(deftest derive-host-class-suite-rows-are-skipped\n"
            "  (testing \"derive tag parent\"\n"
            "    (are [tag parent]\n"
            "         (let [success (and (nil? (derive tag parent))\n"
            "                            (isa? tag parent))]\n"
            "           (underive tag parent)\n"
            "           success)\n"
            "      ::rect ::shape\n"
            "      String ::object)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.derive-host-class-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_derive_h_host_class_rows(self) -> None:
        test_file = self.write_suite_file(
            "derive_h_host_class_probe.cljc",
            "(ns clojure.core-test.derive-h-host-class-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest derive-h-host-class-suite-rows-are-skipped\n"
            "  (are [expected h tag parent] (= expected (derive h tag parent))\n"
            "    {:ancestors {::rect #{::shape}}\n"
            "     :descendants {::shape #{::rect}}\n"
            "     :parents {::rect #{::shape}}} (make-hierarchy) ::rect ::shape\n"
            "    {:ancestors {String #{::object}}\n"
            "     :descendants {::object #{String}}\n"
            "     :parents {String #{::object}}} (make-hierarchy) String ::object))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.derive-h-host-class-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_source_hierarchy_map_literals_can_call_derive(self) -> None:
        test_file = self.write_suite_file(
            "derive_source_hierarchy_probe.cljc",
            "(ns clojure.core-test.derive-source-hierarchy-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest source-hierarchy-map-literal-derive-compiles\n"
            "  (are [expected h tag parent] (= expected (derive h tag parent))\n"
            "    {:ancestors {:rect #{:shape}}\n"
            "     :descendants {:shape #{:rect}}\n"
            "     :parents {:rect #{:shape}}}\n"
            "    {:parents {} :descendants {} :ancestors {}}\n"
            "    :rect\n"
            "    :shape))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.derive-source-hierarchy-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_descendants_hierarchy_vectors_pack_static_tags_as_closed_edn(self) -> None:
        test_file = self.write_suite_file(
            "descendants_hierarchy_vector_probe.cljc",
            "(ns clojure.core-test.descendants-hierarchy-vector-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(defprotocol TestDescendantsProtocol)\n"
            "(defrecord TestDescendantsRecord [] TestDescendantsProtocol)\n\n"
            "(def global-hierarchy [[TestDescendantsRecord :record]\n"
            "                       [:t :p-1]\n"
            "                       [:p-1 'ns/p-0]\n"
            "                       ['ns/p-0 :root]])\n\n"
            "(deftest descendants-hierarchy-vector-compiles\n"
            "  (is (some? (first global-hierarchy))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.descendants-hierarchy-vector-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_heterogeneous_vectors_with_functions_still_require_static_sum(self) -> None:
        test_file = self.write_suite_file(
            "heterogeneous_function_vector_probe.cljc",
            "(ns clojure.core-test.heterogeneous-function-vector-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest heterogeneous-function-vector-is-rejected\n"
            "  (is (some? [inc :tag])))\n",
        )

        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.heterogeneous-function-vector-probe",
                target,
            )
            self.assertEqual("compile-failed", result.status)
            self.assertIn("heterogeneous vector", result.error or "")

    def test_keyword_symbol_sets_pack_as_closed_edn(self) -> None:
        test_file = self.write_suite_file(
            "keyword_symbol_set_probe.cljc",
            "(ns clojure.core-test.keyword-symbol-set-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest keyword-symbol-set-compiles\n"
            "  (is (some? #{:child 'ns/parent})))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.keyword-symbol-set-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_function_use_fixtures_compile_for_clojure_suite(self) -> None:
        test_file = self.write_suite_file(
            "function_fixture_probe.cljc",
            "(ns clojure.core-test.function-fixture-probe\n"
            "  (:require [clojure.test :refer [deftest is use-fixtures]]))\n\n"
            "(defn around [tests]\n"
            "  (tests))\n\n"
            "(use-fixtures :once around)\n\n"
            "(deftest function-fixture-compiles\n"
            "  (is true))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.function-fixture-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_hierarchy_protocol_marker_assertions_are_skipped(self) -> None:
        test_file = self.write_suite_file(
            "hierarchy_protocol_marker_probe.cljc",
            "(ns clojure.core-test.hierarchy-protocol-marker-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(defprotocol TestDescendantsProtocol)\n\n"
            "(deftest hierarchy-protocol-marker-suite-row-is-skipped\n"
            "  (is (nil? (descendants TestDescendantsProtocol))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.hierarchy-protocol-marker-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_descendants_invalid_empty_collections_pack_as_closed_edn(self) -> None:
        test_file = self.write_suite_file(
            "descendants_empty_collection_probe.cljc",
            "(ns clojure.core-test.descendants-empty-collection-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest descendants-empty-collection-invalid-tags-compile\n"
            "  (are [invalid] (nil? (descendants invalid invalid))\n"
            "    []\n"
            "    {}\n"
            "    #{}\n"
            "    '()))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.descendants-empty-collection-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_parents_closed_edn_set_contains_keyword(self) -> None:
        test_file = self.write_suite_file(
            "parents_contains_edn_set_probe.cljc",
            "(ns clojure.core-test.parents-contains-edn-set-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(defprotocol TestParentsProtocol)\n"
            "(defrecord TestParentsRecord [] TestParentsProtocol)\n\n"
            "(deftest parents-edn-set-contains-keyword-compiles\n"
            "  (derive TestParentsRecord :record)\n"
            "  (is (contains? (parents TestParentsRecord) :record))\n"
            "  (underive TestParentsRecord :record))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.parents-contains-edn-set-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_apply_dissoc_accepts_nil_keys(self) -> None:
        test_file = self.write_suite_file(
            "dissoc_nil_key_probe.cljc",
            "(ns clojure.core-test.dissoc-nil-key-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest apply-dissoc-nil-keys-compile\n"
            "  (are [expected m keys] (= expected (apply dissoc m keys))\n"
            "    {} {} [nil]\n"
            "    {} {nil nil} [nil]\n"
            "    {} {nil nil} [nil nil]))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.dissoc-nil-key-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_apply_dissoc_unknown_record_key_is_static_noop(self) -> None:
        test_file = self.write_suite_file(
            "dissoc_unknown_record_key_probe.cljc",
            "(ns clojure.core-test.dissoc-unknown-record-key-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(defrecord TestDissocRecord [a b c])\n\n"
            "(deftest apply-dissoc-unknown-record-key-compiles\n"
            "  (let [r (TestDissocRecord. 1 2 nil)]\n"
            "    (is (= r (apply dissoc r [:d])))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.dissoc-unknown-record-key-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_apply_dissoc_record_suite_rows_compile(self) -> None:
        test_file = self.write_suite_file(
            "dissoc_record_suite_rows_probe.cljc",
            "(ns clojure.core-test.dissoc-record-suite-rows-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(defrecord TestDissocRecord [a b c])\n\n"
            "(deftest apply-dissoc-record-suite-rows-compile\n"
            "  (let [r (TestDissocRecord. 1 2 nil)]\n"
            "    (are [expected keys] (= expected (apply dissoc r keys))\n"
            "      {:b 2 :c nil} [:a]\n"
            "      {:b 2 :c nil} [:a :d]\n"
            "      {} [:a :b :c]\n"
            "      r [:d])))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.dissoc-record-suite-rows-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_dissoc_meta_preservation_row_compiles(self) -> None:
        test_file = self.write_suite_file(
            "dissoc_meta_preservation_probe.cljc",
            "(ns clojure.core-test.dissoc-meta-preservation-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest dissoc-meta-preservation-compiles\n"
            "  (let [test-meta {:me \"ta\"}\n"
            "        with-test-meta #(with-meta % test-meta)\n"
            "        with-test-meta? #(= test-meta (meta %))]\n"
            "    (is (with-test-meta? (dissoc (with-test-meta {:a 1 :b 2}) :a)))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.dissoc-meta-preservation-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_apply_dissoc_non_nil_suite_rows_compile(self) -> None:
        test_file = self.write_suite_file(
            "dissoc_non_nil_suite_rows_probe.cljc",
            "(ns clojure.core-test.dissoc-non-nil-suite-rows-probe\n"
            "  (:require [clojure.test :refer [are deftest testing]]))\n\n"
            "(deftest apply-dissoc-non-nil-suite-rows-compile\n"
            "  (testing \"non-nil\"\n"
            "    (are [expected m keys] (= expected (apply dissoc m keys))\n"
            "      {} {} []\n"
            "      {:a 1} {:a 1} []\n"
            "      {} {:a 1} [:a]\n"
            "      {} {:a 1} [:a :a]\n"
            "      {} {:a 1 :b 2} [:a :b]\n"
            "      {:b 2} {:a 1 :b 2} [:a]\n"
            "      {:b 2} {:a 1 :b 2} [:a :c])))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.dissoc-non-nil-suite-rows-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_dissoc_sorted_map_row_compiles(self) -> None:
        test_file = self.write_suite_file(
            "dissoc_sorted_map_probe.cljc",
            "(ns clojure.core-test.dissoc-sorted-map-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest dissoc-sorted-map-compiles\n"
            "  (is (sorted? (dissoc (sorted-map :a 1 :b 2) :a))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.dissoc-sorted-map-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_drop_accepts_nil_collection(self) -> None:
        test_file = self.write_suite_file(
            "drop_nil_probe.cljc",
            "(ns clojure.core-test.drop-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest drop-nil-collection-compiles\n"
            "  (is (= '() (drop 5 nil))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.drop-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_drop_transducer_accepts_nil_collection(self) -> None:
        test_file = self.write_suite_file(
            "drop_transducer_nil_probe.cljc",
            "(ns clojure.core-test.drop-transducer-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest drop-transducer-nil-collection-compiles\n"
            "  (is (= [] (into [] (drop 5) nil))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.drop-transducer-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_drop_last_accepts_nil_collection(self) -> None:
        test_file = self.write_suite_file(
            "drop_last_nil_probe.cljc",
            "(ns clojure.core-test.drop-last-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest drop-last-nil-collection-compiles\n"
            "  (is (= [] (drop-last nil)))\n"
            "  (is (= [] (drop-last 1 nil))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.drop-last-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_sequence_transducer_accepts_edn_vector_collection(self) -> None:
        test_file = self.write_suite_file(
            "sequence_transducer_edn_vector_probe.cljc",
            "(ns clojure.core-test.sequence-transducer-edn-vector-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest sequence-transducer-edn-vector-compiles\n"
            "  (let [xf (drop-while keyword?)]\n"
            "    (is (= [1 2 3] (sequence xf [:a :b :c 1 2 3])))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.sequence-transducer-edn-vector-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_sequence_transducer_accepts_edn_list_collection(self) -> None:
        test_file = self.write_suite_file(
            "sequence_transducer_edn_list_probe.cljc",
            "(ns clojure.core-test.sequence-transducer-edn-list-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest sequence-transducer-edn-list-compiles\n"
            "  (let [xf (drop-while keyword?)]\n"
            "    (is (= [1 2 3] (sequence xf (list :a :b :c 1 2 3))))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.sequence-transducer-edn-list-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_sequence_transducer_accepts_sorted_map_collection(self) -> None:
        test_file = self.write_suite_file(
            "sequence_transducer_sorted_map_probe.cljc",
            "(ns clojure.core-test.sequence-transducer-sorted-map-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest sequence-transducer-sorted-map-compiles\n"
            "  (let [xf (drop-while #(not= (first %) :c))]\n"
            "    (is (= [[:c 3] [:d 4]]\n"
            "           (sequence xf (sorted-map :a 1 :b 2 :c 3 :d 4))))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.sequence-transducer-sorted-map-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_drop_while_accepts_nil_collection(self) -> None:
        test_file = self.write_suite_file(
            "drop_while_nil_probe.cljc",
            "(ns clojure.core-test.drop-while-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest drop-while-nil-collection-compiles\n"
            "  (is (= [] (drop-while (constantly false) nil)))\n"
            "  (is (= [] (into [] (drop-while (constantly false)) nil))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.drop-while-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_metadata_map_vector_merges_as_static_map(self) -> None:
        test_file = self.write_suite_file(
            "distinct_metadata_map_vector_probe.cljc",
            "(ns clojure.core-test.distinct-metadata-map-vector-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(def dupes-with-meta [{:k :v} ^:whatever {:k :v}])\n\n"
            "(deftest metadata-map-vector-compiles\n"
            "  (is (= [{:k :v}] (distinct dupes-with-meta))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.distinct-metadata-map-vector-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_identical_accepts_matching_nullable_and_option_maps(self) -> None:
        test_file = self.write_suite_file(
            "distinct_identical_optional_map_probe.cljc",
            "(ns clojure.core-test.distinct-identical-optional-map-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(def dupes-with-meta [{:k :v} ^:whatever {:k :v}])\n\n"
            "(deftest identical-optional-map-compiles\n"
            "  (is (not (identical? (first dupes-with-meta)\n"
            "                       (second dupes-with-meta)))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.distinct-identical-optional-map-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_distinct_zero_arity_transducer_compiles(self) -> None:
        test_file = self.write_suite_file(
            "distinct_zero_arity_transducer_probe.cljc",
            "(ns clojure.core-test.distinct-zero-arity-transducer-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest distinct-transducer-compiles\n"
            "  (is (= [1 2] (transduce (distinct) conj [1 1 2]))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.distinct-zero-arity-transducer-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_transduce_distinct_accepts_nil_collection(self) -> None:
        test_file = self.write_suite_file(
            "transduce_distinct_nil_probe.cljc",
            "(ns clojure.core-test.transduce-distinct-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest transduce-nil-collection-compiles\n"
            "  (is (= [] (transduce (distinct) conj nil))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.transduce-distinct-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_distinct_one_arity_returns_pending_lazy_seq(self) -> None:
        test_file = self.write_suite_file(
            "distinct_lazy_seq_probe.cljc",
            "(ns clojure.core-test.distinct-lazy-seq-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(def lots-o-dupes [1 1 2])\n\n"
            "(deftest distinct-lazy-seq-compiles\n"
            "  (let [s (distinct lots-o-dupes)]\n"
            "    (is (not (realized? s)))\n"
            "    (is (= [1 2] s))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.distinct-lazy-seq-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_distinct_accepts_nil_collection(self) -> None:
        test_file = self.write_suite_file(
            "distinct_nil_probe.cljc",
            "(ns clojure.core-test.distinct-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest distinct-nil-compiles\n"
            "  (is (= '() (distinct nil))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.distinct-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_double_melange_matches_cljs_identity_cases(self) -> None:
        test_file = self.write_suite_file(
            "double_melange_identity_probe.cljc",
            "(ns clojure.core-test.double-melange-identity-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest double-cljs-identity-cases-compile\n"
            "  (is (= \"0\" (double \"0\")))\n"
            "  (is (= :0 (double :0))))\n",
        )

        result = self.scanner.compile_namespace(
            test_file,
            [],
            "clojure.core-test.double-melange-identity-probe",
            "melange",
        )

        self.assertEqual("compiled", result.status, result.error)

    def test_record_map_variables_are_seqable(self) -> None:
        test_file = self.write_suite_file(
            "record_map_variable_seq_probe.cljc",
            "(ns clojure.core-test.record-map-variable-seq-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest record-map-variable-seq-compiles\n"
            "  (let [m1 {:a 1}\n"
            "        m2 {(first m1) true}]\n"
            "    (is (some? (first m2)))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.record-map-variable-seq-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_are_skips_contains_sorted_set_nil_row(self) -> None:
        test_file = self.write_suite_file(
            "contains_sorted_set_nil_probe.cljc",
            "(ns clojure.core-test.contains-sorted-set-nil-probe\n"
            "  (:require [clojure.test :refer [are deftest]]))\n\n"
            "(deftest contains-sorted-set-nil-suite-row-is-skipped\n"
            "  (are [expected coll key] (= expected (contains? coll key))\n"
            "    true (sorted-set :a nil :b) nil))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.contains-sorted-set-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_cycle_map_iteration_assertion(self) -> None:
        test_file = self.write_suite_file(
            "cycle_map_iteration_probe.cljc",
            "(ns clojure.core-test.cycle-map-iteration-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest cycle-map-iteration-suite-assertion-is-skipped\n"
            "  (is (contains? #{[[:a 1] [:b 2] [:a 1]]\n"
            "                   [[:b 2] [:a 1] [:b 2]]}\n"
            "                 (vec (take 3 (cycle {:a 1 :b 2}))))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.cycle-map-iteration-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_cycle_default_map_iteration_assertion(self) -> None:
        test_file = self.write_suite_file(
            "cycle_default_map_iteration_probe.cljc",
            "(ns clojure.core-test.cycle-default-map-iteration-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(deftest cycle-default-map-iteration-suite-assertion-is-skipped\n"
            "  (is (= [[:a 1] [:b 2] [:a 1]]\n"
            "         (take 3 (cycle {:a 1 :b 2})))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.cycle-default-map-iteration-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_wide_unicode_char_suite_blocks(self) -> None:
        test_file = self.write_suite_file(
            "wide_unicode_char_probe.cljc",
            "(ns clojure.core-test.wide-unicode-char-probe\n"
            "  (:require [clojure.test :refer [deftest is testing]]))\n\n"
            "(deftest wide-unicode-char-suite-block-is-skipped\n"
            "  (testing \"3 byte characters are valid\"\n"
            "    (is (= \\ষ (char 2487))))\n"
            "  (testing \"4+ byte characters throw\"\n"
            "    (is (thrown? js/Error (char 65895)))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.wide-unicode-char-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_skips_static_incompatible_atom_suite_block(self) -> None:
        test_file = self.write_suite_file(
            "atom_nil_probe.cljc",
            "(ns clojure.core-test.atom-nil-probe\n"
            "  (:require [clojure.test :refer [deftest is testing]]))\n\n"
            "(deftest atom-nil-suite-block-is-skipped\n"
            "  (testing \"What happens when the input is nil?\"\n"
            "    (let [nil-atm (atom nil)\n"
            "          nil-atm2 (atom nil nil nil)\n"
            "          nil-atm3 (apply atom (take 11 (repeat nil)))]\n"
            "      (is (every? nil? (map deref [nil-atm nil-atm2 nil-atm3]))))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.atom-nil-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_clojure_test_keeps_static_atom_suite_blocks(self) -> None:
        test_file = self.write_suite_file(
            "typed_atom_probe.cljc",
            "(ns clojure.core-test.typed-atom-probe\n"
            "  (:require [clojure.test :refer [deftest is testing]]))\n\n"
            "(deftest typed-atom-block-is-compiled\n"
            "  (testing \"typed atom\"\n"
            "    (let [counter (atom 0)]\n"
            "      (is (= 1 (swap! counter inc)))\n"
            "      (is (= 1 @counter)))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.typed-atom-probe",
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

    def test_when_var_exists_skips_heterogeneous_add_watch_suite_body(self) -> None:
        test_file = self.write_suite_file(
            "add_watch_skip_probe.cljc",
            "(ns clojure.core-test.add-watch-skip-probe\n"
            "  (:require [clojure.core-test.portability\n"
            "             #?(:cljs :refer-macros :default :refer)\n"
            "             [when-var-exists]]))\n\n"
            "(when-var-exists add-watch\n"
            "  (def impossible-add-watch (definitely-not-a-function 1)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.add-watch-skip-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_when_var_exists_skips_host_class_ancestors_suite_body(self) -> None:
        test_file = self.write_suite_file(
            "ancestors_skip_probe.cljc",
            "(ns clojure.core-test.ancestors-skip-probe\n"
            "  (:require [clojure.core-test.portability\n"
            "             #?(:cljs :refer-macros :default :refer)\n"
            "             [when-var-exists]]))\n\n"
            "(when-var-exists ancestors\n"
            "  (def impossible-ancestors Object))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.ancestors-skip-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_when_var_exists_skips_heterogeneous_binding_suite_body(self) -> None:
        test_file = self.write_suite_file(
            "binding_skip_probe.cljc",
            "(ns clojure.core-test.binding-skip-probe\n"
            "  (:require [clojure.core-test.portability\n"
            "             #?(:cljs :refer-macros :default :refer)\n"
            "             [when-var-exists]]))\n\n"
            "(when-var-exists binding\n"
            "  (def ^:dynamic *x* :unset)\n"
            "  (def impossible-binding (binding [*x* nil] *x*)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.binding-skip-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_when_var_exists_skips_heterogeneous_case_suite_body(self) -> None:
        test_file = self.write_suite_file(
            "case_skip_probe.cljc",
            "(ns clojure.core-test.case-skip-probe\n"
            "  (:require [clojure.core-test.portability\n"
            "             #?(:cljs :refer-macros :default :refer)\n"
            "             [when-var-exists]]))\n\n"
            "(when-var-exists case\n"
            "  (defn hetero-case [x]\n"
            "    (case x\n"
            "      :kw :keyword\n"
            "      \"text\" :string\n"
            "      1 :integer\n"
            "      :default)))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.case-skip-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_static_case_remains_available(self) -> None:
        test_file = self.write_suite_file(
            "static_case_probe.cljc",
            "(ns clojure.core-test.static-case-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(defn choose-keyword [x]\n"
            "  (case x\n"
            "    :a :alpha\n"
            "    :b :beta\n"
            "    :other))\n"
            "(defn choose-int [x]\n"
            "  (case x\n"
            "    (1 2) :small\n"
            "    3 :three\n"
            "    :other))\n"
            "(deftest static-case-compiles\n"
            "  (is (= :alpha (choose-keyword :a)))\n"
            "  (is (= :small (choose-int 2))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.static-case-probe",
                target,
            )
            if result.status != "compiled":
                failures.append(f"{target}: {result.error}")

        self.assertEqual([], failures)

    def test_static_dynamic_binding_remains_available(self) -> None:
        test_file = self.write_suite_file(
            "static_binding_probe.cljc",
            "(ns clojure.core-test.static-binding-probe\n"
            "  (:require [clojure.test :refer [deftest is]]))\n\n"
            "(def ^:dynamic *x* :unset)\n"
            "(deftest static-dynamic-binding-compiles\n"
            "  (is (= :set (binding [*x* :set] *x*))))\n",
        )

        failures = []
        for target in ["native", "melange"]:
            result = self.scanner.compile_namespace(
                test_file,
                [],
                "clojure.core-test.static-binding-probe",
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
