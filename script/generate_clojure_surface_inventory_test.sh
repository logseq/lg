#!/bin/sh
set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

mkdir -p "$tmp/logseq/src"
cat >"$tmp/logseq/src/example.cljs" <<'EOF'
(ns example
  (:require [clojure.string :as string]
            [clojure.set :refer [union]]
            [clojure.walk :as walk]
            [clojure.zip :as zip]))

(string/upper-case "logseq")
(union #{1} #{2})
(walk/postwalk identity {})
(zip/root nil)
(cljs.core/identity 1)
EOF

"$root/script/generate_clojure_surface_inventory.sh" \
  "$root" "$tmp/logseq" >"$tmp/inventory.tsv"

awk -F '\t' '$1 == "compiler-call" && ($2 == "identity" || $3 == "source-shadowed") {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "+" && $3 == "typed-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "-" && $3 == "typed-primitive" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "binding" && $3 == "special-form" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '
  $1 == "compiler-call" && $3 == "blocked-static-typing" &&
  ($4 == "" || $4 == "requires-variadic-dependent-lazy-or-capability-type-support") {
    print "blocked compiler call lacks a concrete reason: " $2 > "/dev/stderr"
    failed=1
  }
  END {exit failed}
' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "compiler-call" && $2 == "Buffer.t" {found=1} END {exit found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.data" && $3 == "compiler-owned" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.string" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace" && $2 == "clojure.edn" && $3 == "source-with-primitive-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "namespace-var" && $2 == "clojure.data/diff" && $3 == "host-boundary" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "runtime-primitive" && $2 == "Lg_runtime.Runtime_string.split" {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace" && $2 == "clojure.string" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace" && $2 == "clojure.set" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var" && $2 == "clojure.string/upper-case" && $3 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.string" && $3 == "source-aggregate" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.walk" && $3 == "blocked-static-typing" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-namespace-status" && $2 == "clojure.zip" && $3 == "unsupported" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.walk/postwalk" && $3 == "blocked-static-typing" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "clojure.zip/root" && $3 == "unsupported" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
awk -F '\t' '$1 == "logseq-qualified-var-status" && $2 == "cljs.core/identity" && $3 == "source-core-alias" && $4 == 1 {found=1} END {exit !found}' "$tmp/inventory.tsv"
