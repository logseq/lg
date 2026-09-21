#!/bin/sh
set -eu
cli="$1"
ocamlc="$2"
runtime_cmi="$3"
rrb_cmi="$4"
work=$(mktemp -d "${TMPDIR:-/tmp}/lg-prefix-interface.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
export LG_CACHE_DIR="$work/cache"
cat >"$work/base.cljc" <<'SOURCE'
(ns prefix.base)
(type-record counter (value :int))
(defn make-counter [value] (record counter (value value)))
(defn identity-value [value] value)
SOURCE
cat >"$work/use.cljc" <<'SOURCE'
(ns prefix.use (:require [prefix.base :as base]))
(def item (base/make-counter 41))
(def answer (base/identity-value (:value item)))
(def text (base/identity-value "checked"))
SOURCE
"$cli" --compile-files-state "$work/base.state" "$work/base.cljc" -o "$work/prefix.ml"
"$ocamlc" -w -a -I "$runtime_cmi" -I "$rrb_cmi" -c "$work/prefix.ml"
compile_suffix() {
  "$cli" --compile-files-chunk-from "$work/base.state" \
    --prefix-interface "$work/prefix.cmi" "$work/use.cljc" -o "$work/use.ml"
}
compile_suffix
"$ocamlc" -w -a -I "$work" -I "$runtime_cmi" -I "$rrb_cmi" -c "$work/use.ml"
compile_suffix
cat >"$work/invalid.cljc" <<'SOURCE'
(ns prefix.invalid (:require [prefix.base :as base]))
(def item (base/make-counter "wrong"))
SOURCE
if "$cli" --compile-files-chunk-from "$work/base.state" \
  --prefix-interface "$work/prefix.cmi" "$work/invalid.cljc" -o "$work/invalid.ml" \
  >"$work/out" 2>"$work/err"; then
  echo "prefix interface bypassed static type checking" >&2; exit 1
fi
# Replacing the CMI at the same path must invalidate the warm suffix cache.
printf 'let unrelated = 0\n' >"$work/prefix.ml"
"$ocamlc" -c "$work/prefix.ml"
if compile_suffix >"$work/out" 2>"$work/err"; then
  echo "changed prefix interface reused an incompatible cached suffix" >&2; exit 1
fi
if ! grep -q 'Unbound value' "$work/err"; then cat "$work/err" >&2; exit 1; fi
rm "$work/prefix.cmi"
if compile_suffix >"$work/out" 2>"$work/err"; then
  echo "missing prefix interface unexpectedly compiled" >&2; exit 1
fi
if grep -q 'Fatal error' "$work/err"; then cat "$work/err" >&2; exit 1; fi
printf 'invalid interface\n' >"$work/prefix.cmi"
if compile_suffix >"$work/out" 2>"$work/err"; then
  echo "invalid prefix interface unexpectedly compiled" >&2; exit 1
fi
if grep -q 'Fatal error' "$work/err"; then cat "$work/err" >&2; exit 1; fi
