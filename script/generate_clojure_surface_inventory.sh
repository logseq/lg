#!/bin/sh
set -eu

if test "$#" -lt 1 || test "$#" -gt 3; then
  echo "usage: $0 LG_ROOT [LOGSEQ_ROOT] [CLOJURESCRIPT_ROOT]" >&2
  exit 2
fi

lg_root=$(CDPATH= cd -- "$1" && pwd)
logseq_root=${2-}
if test -n "$logseq_root"; then
  logseq_root=$(CDPATH= cd -- "$logseq_root" && pwd)
fi
clojurescript_root=${3-}
if test -n "$clojurescript_root"; then
  clojurescript_root=$(CDPATH= cd -- "$clojurescript_root" && pwd)
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

if test -n "$clojurescript_root"; then
  checkout_commit=$(git -C "$clojurescript_root" rev-parse HEAD)
  printf 'meta\tclojurescript-checkout-commit\t%s\n' "$checkout_commit"
  if test "$checkout_commit" != "$upstream_commit"; then
    echo "ClojureScript checkout does not match stdlib/upstream.edn: expected $upstream_commit, found $checkout_commit" >&2
    exit 1
  fi
fi

# Parse the OCaml AST and select the largest `match name with` expression. This
# avoids treating string patterns from nested type/argument matches as public
# compiler dispatch names.
ocaml -I +compiler-libs ocamlcommon.cma \
  "$lg_root/script/extract_ocaml_string_dispatch.ml" "$call_elaborator" \
  >"$tmp/compiler-calls"

dispatch_count=$(wc -l <"$tmp/compiler-calls" | tr -d ' ')
if test "$dispatch_count" -ne 314; then
  echo "compiler call dispatch changed: expected 314 names, found $dispatch_count" >&2
  echo "review and classify every added or removed name before updating the count" >&2
  exit 1
fi

awk '
  BEGIN {
    split("binding with-open with-out-str reify assert delay set! throw", xs)
    for (i in xs) special[xs[i]] = 1
    split("apply assoc-in comp concat constantly cycle doall dorun drop drop-while every-pred filter filterv fnil get-in group-by interleave into juxt keep map map-indexed mapcat mapv max max-key merge min min-key next not-empty partial partition partition-all partition-by reduce reduce-kv reductions remove repeat repeatedly rest rseq run! select-keys some some-fn sort sort-by take take-nth take-while update-in vals vec", xs)
    for (i in xs) blocked[xs[i]] = 1
    blocked_reason["apply"] = "variadic-apply-requires-dependent-fixed-arguments-and-final-sequence-expansion"
    split("assoc-in get-in update-in", xs)
    for (i in xs) blocked_reason[xs[i]] = "nested-map-paths-require-dependent-key-and-value-types"
    split("comp constantly every-pred fnil juxt partial some-fn", xs)
    for (i in xs) blocked_reason[xs[i]] = "returned-variadic-or-overloaded-function-types-are-not-source-expressible"
    split("concat interleave map mapv", xs)
    for (i in xs) blocked_reason[xs[i]] = "variadic-multi-collection-arities-and-lazy-or-transducer-cases-are-not-source-expressible"
    split("cycle drop drop-while filter keep map-indexed mapcat remove repeat repeatedly take take-while", xs)
    for (i in xs) blocked_reason[xs[i]] = "upstream-lazy-sequence-or-transducer-behavior-is-not-source-expressible"
    split("doall dorun run!", xs)
    for (i in xs) blocked_reason[xs[i]] = "sequence-realization-and-effect-order-remain-a-compiler-runtime-boundary"
    blocked_reason["filterv"] = "generic-seqable-callback-projection-emits-an-unbound-capability-witness"
    blocked_reason["group-by"] = "nested-seqable-callback-capability-projection-conflates-logical-items-with-witness-storage"
    blocked_reason["into"] = "target-collection-representation-and-transducer-overload-require-dependent-types"
    split("max max-key min min-key", xs)
    for (i in xs) blocked_reason[xs[i]] = "variadic-comparable-types-and-key-callback-overloads-are-not-source-expressible"
    blocked_reason["merge"] = "variadic-map-and-record-shape-unification-is-not-source-expressible"
    split("next rest", xs)
    for (i in xs) blocked_reason[xs[i]] = "nil-versus-empty-sequence-semantics-remain-a-collection-capability-boundary"
    blocked_reason["not-empty"] = "nullable-result-must-preserve-the-input-concrete-collection-type"
    split("partition partition-all", xs)
    for (i in xs) blocked_reason[xs[i]] = "multi-arity-lazy-padding-and-transducer-cases-are-not-source-expressible"
    blocked_reason["partition-by"] = "lazy-partitions-and-generic-key-capability-cannot-yet-share-one-source-signature"
    split("reduce reduce-kv reductions", xs)
    for (i in xs) blocked_reason[xs[i]] = "multi-arity-reduced-short-circuit-and-collection-specific-callback-typing-remain-compiler-owned"
    blocked_reason["rseq"] = "reversible-protocol-dispatch-and-nil-on-unsupported-types-are-not-source-expressible"
    blocked_reason["select-keys"] = "map-or-record-key-projection-requires-a-dependent-result-shape"
    blocked_reason["some"] = "nullable-first-truthy-result-needs-a-generic-witness-through-the-sequence-loop"
    split("sort sort-by", xs)
    for (i in xs) blocked_reason[xs[i]] = "comparator-overloads-and-seqable-capability-adaptation-remain-compiler-owned"
    blocked_reason["take-nth"] = "one-arity-stateful-transducer-and-lazy-two-arity-sequence-are-not-source-expressible"
    blocked_reason["vals"] = "map-and-structural-record-value-projection-needs-a-closed-value-sum"
    blocked_reason["vec"] = "generic-seqable-to-concrete-vector-conversion-requires-representation-capability"
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
    } else if (blocked[$0]) {
      classification = "blocked-static-typing"
      reason = blocked_reason[$0]
      if (reason == "") {
        print "missing concrete blocker reason for " $0 > "/dev/stderr"
        exit 1
      }
    } else if (primitive[$0]) {
      classification = "typed-primitive"
      reason = "static-scalar-primitive"
    } else if (host[$0] || $0 ~ /^\./ || $0 ~ /^js\// || $0 ~ /^__/ || $0 ~ /^-/) {
      classification = "host-boundary"
      reason = "host-interop-or-runtime-effect-boundary"
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
  if git -C "$logseq_root" rev-parse HEAD >"$tmp/logseq-commit" 2>/dev/null; then
    printf 'meta\tlogseq-commit\t%s\n' "$(sed -n '1p' "$tmp/logseq-commit")"
  fi
  sed -n '/:aggregate-namespaces/,/]/p' "$lg_root/stdlib/upstream.edn" \
    | tr ' []' '\n' \
    | awk '/^(clojure|cljs)\./ {
        print $1 "\tsource-aggregate\taggregate-stdlib"
        if ($1 == "clojure.core") {
          print "cljs.core\tsource-core-alias\tautomatic-core-alias"
        }
      }' >"$tmp/namespace-support"

  awk '
    /^  (clojure|cljs)\.[A-Za-z0-9_.-]+$/ {
      namespace = $1
      blocked = 0
    }
    /:status :blocked/ {blocked = 1}
    blocked && /:reason :[A-Za-z0-9_.-]+/ {
      reason = $2
      sub(/^:/, "", reason)
      sub(/[^A-Za-z0-9_.-].*$/, "", reason)
      print namespace "\tblocked-static-typing\t" reason
      blocked = 0
    }
  ' "$lg_root/stdlib/upstream.edn" >>"$tmp/namespace-support"

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
    | LC_ALL=C sort -t '	' -k1,1 -k3,3nr -k2,2 \
    >"$tmp/logseq-counts" || true

  awk -F '\t' '
    FNR == NR {
      support[$1] = $2
      reason[$1] = $3
      next
    }
    function namespace_status(namespace) {
      return namespace in support ? support[namespace] : "unsupported"
    }
    function namespace_reason(namespace) {
      return namespace in reason ? reason[namespace] : "not-in-aggregate-or-blocked-manifest"
    }
    {
      print
      if ($1 == "logseq-namespace") {
        print "logseq-namespace-status\t" $2 "\t" namespace_status($2) \
          "\t" $3 "\t" namespace_reason($2)
      } else if ($1 == "logseq-qualified-var") {
        split($2, qualified, "/")
        namespace = qualified[1]
        print "logseq-qualified-var-status\t" $2 "\t" \
          namespace_status(namespace) "\t" $3 "\t" \
          namespace_reason(namespace)
      }
    }
  ' "$tmp/namespace-support" "$tmp/logseq-counts" \
    | LC_ALL=C sort -t '	' -k1,1 -k2,2
fi
