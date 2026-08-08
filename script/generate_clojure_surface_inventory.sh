#!/bin/sh
set -eu

if test "$#" -lt 1 || test "$#" -gt 2; then
  echo "usage: $0 LG_ROOT [LOGSEQ_ROOT]" >&2
  exit 2
fi

lg_root=$(CDPATH= cd -- "$1" && pwd)
logseq_root=${2-}
if test -n "$logseq_root"; then
  logseq_root=$(CDPATH= cd -- "$logseq_root" && pwd)
fi
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT HUP INT TERM

call_elaborator="$lg_root/src/call_elaborator.ml"
core_namespaces="$lg_root/src/core_namespaces.ml"

if ! test -f "$call_elaborator" || ! test -f "$core_namespaces"; then
  echo "LG_ROOT must contain src/call_elaborator.ml and src/core_namespaces.ml" >&2
  exit 2
fi

upstream_commit=$(sed -n 's/.*:commit "\([0-9a-f][0-9a-f]*\)".*/\1/p' \
  "$lg_root/stdlib/upstream.edn" | head -1)
printf 'meta\tclojurescript-commit\t%s\n' "$upstream_commit"

# Parse the OCaml AST and select the largest `match name with` expression. This
# avoids treating string patterns from nested type/argument matches as public
# compiler dispatch names.
ocaml -I +compiler-libs ocamlcommon.cma \
  "$lg_root/script/extract_ocaml_string_dispatch.ml" "$call_elaborator" \
  >"$tmp/compiler-calls"

dispatch_count=$(wc -l <"$tmp/compiler-calls" | tr -d ' ')
if test "$dispatch_count" -ne 339; then
  echo "compiler call dispatch changed: expected 339 names, found $dispatch_count" >&2
  echo "review and classify every added or removed name before updating the count" >&2
  exit 1
fi

awk '
  BEGIN {
    split("binding with-open with-out-str reify assert delay set! throw", xs)
    for (i in xs) special[xs[i]] = 1
    split("identity complement not-any? not-every? even? odd? bit-clear bit-flip bit-set bit-test", xs)
    for (i in xs) shadowed[xs[i]] = 1
    split("apply assoc-in boolean bounded-count butlast comp concat constantly cycle dedupe distinct doall dorun drop drop-last drop-while every-pred every? ffirst filter filterv fnil fnext get-in group-by interleave interpose into juxt keep map map-indexed mapcat mapv max max-key merge min min-key next nfirst nnext not-empty nthnext nthrest partial partition partition-all partition-by reduce reduce-kv reductions remove repeat repeatedly rest reverse rseq run! select-keys some some-fn sort sort-by split-at split-with take take-last take-nth take-while update-in vals vec zipmap", xs)
    for (i in xs) blocked[xs[i]] = 1
    split("clj->js clojure.pprint/pprint current-time-millis enable-console-print! ex-info future-call pr pr-sequential-writer pr-str pr-writer print println prn raise requiring-resolve resolve uuid weak-clear! weak-deref weak-ref", xs)
    for (i in xs) host[xs[i]] = 1
    split("+ - * / < <= = == > >= inc dec int long double quot rem mod bit-and bit-or bit-xor bit-not bit-shift-left bit-shift-right", xs)
    for (i in xs) primitive[xs[i]] = 1
  }
  {
    classification = "typed-primitive"
    reason = "static-elaboration-or-minimal-runtime-abi"
    if (special[$0]) {
      classification = "special-form"
      reason = "compiler-owned-syntax-or-control-flow"
    } else if (shadowed[$0]) {
      classification = "source-shadowed"
      reason = "source-stdlib-precedes-legacy-compiler-fallback"
    } else if (blocked[$0]) {
      classification = "blocked-static-typing"
      reason = "requires-variadic-dependent-lazy-or-capability-type-support"
    } else if (host[$0] || $0 ~ /^\./ || $0 ~ /^js\// || $0 ~ /^__/ || $0 ~ /^-/) {
      classification = "host-boundary"
      reason = "host-interop-or-runtime-effect-boundary"
    } else if (primitive[$0]) {
      classification = "typed-primitive"
      reason = "static-scalar-primitive"
    }
    print "compiler-call\t" $0 "\t" classification "\t" reason
  }
' "$tmp/compiler-calls"

for namespace in clojure.core cljs.core clojure.data clojure.edn cljs.reader clojure.string clojure.set clojure.walk; do
  ownership=manifest-only
  if test "$namespace" = clojure.core || test "$namespace" = cljs.core; then
    ownership=compiler-owned
  elif test "$namespace" = clojure.string \
    || test "$namespace" = clojure.edn \
    || test "$namespace" = cljs.reader; then
    ownership=source-with-primitive-boundary
  elif test "$namespace" = clojure.set; then
    ownership=source
  elif grep -F "\"$namespace\"" "$core_namespaces" >/dev/null; then
    ownership=compiler-owned
  fi
  printf 'namespace\t%s\t%s\n' "$namespace" "$ownership"
done

while IFS='|' read -r var classification; do
  printf 'namespace-var\t%s\t%s\n' "$var" "$classification"
done <<'EOF'
clojure.data/diff|host-boundary
clojure.edn/read-string|source
clojure.edn/register-tag-parser!|host-boundary
cljs.reader/read-string|source
cljs.reader/register-tag-parser!|host-boundary
clojure.string/split|host-boundary
clojure.walk/walk|host-boundary
clojure.walk/prewalk|host-boundary
clojure.walk/postwalk|host-boundary
EOF

grep -rhoE 'Lg_runtime\.Runtime_[A-Za-z0-9_]+(\.[a-z][A-Za-z0-9_]*)+' \
  "$lg_root/src" \
  | LC_ALL=C sort -u \
  | awk '{print "runtime-primitive\t" $0 "\ttyped-primitive-boundary"}'

if test -n "$logseq_root" && test -d "$logseq_root"; then
  (
    cd "$logseq_root"
    rg --files -0 -g '*.clj' -g '*.cljs' -g '*.cljc' \
      | xargs -0 -n 100 bb "$lg_root/script/clojure_namespace_inventory.clj"
  ) \
    | LC_ALL=C sort \
    | uniq -c \
    | awk '
        $2 == "namespace" {
          print "logseq-namespace\t" $3 "\t" $1
        }
        $2 == "qualified-var" {
          print "logseq-qualified-var\t" $3 "\t" $1
        }
      ' \
    | LC_ALL=C sort -t '	' -k1,1 -k3,3nr -k2,2 || true
fi
