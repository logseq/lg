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
