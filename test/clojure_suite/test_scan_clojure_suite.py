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


def load_scanner():
    spec = importlib.util.spec_from_file_location("scan_clojure_suite", SCANNER_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load scan_clojure_suite.py")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class ScannerDependencyTests(unittest.TestCase):
    def setUp(self) -> None:
        self.scanner = load_scanner()
        self.tmp = tempfile.TemporaryDirectory(prefix="lg-clojure-suite-test-")
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


if __name__ == "__main__":
    unittest.main()
