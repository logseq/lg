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
expression_elaborator="$lg_root/src/expression_elaborator.ml"
type_inference="$lg_root/src/type_inference.ml"
core_namespaces="$lg_root/src/core_namespaces.ml"

if ! test -f "$call_elaborator" \
  || ! test -f "$expression_elaborator" \
  || ! test -f "$type_inference" \
  || ! test -f "$core_namespaces"; then
  echo "LG_ROOT must contain the compiler elaborators and src/core_namespaces.ml" >&2
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
if test "$dispatch_count" -ne 195; then
  echo "compiler call dispatch changed: expected 195 names, found $dispatch_count" >&2
  echo "review and classify every added or removed name before updating the count" >&2
  exit 1
fi

awk '
  BEGIN {
    split("binding with-open with-out-str reify assert delay set! throw", xs)
    for (i in xs) special[xs[i]] = 1
    split("apply assoc-in comp concat doall drop drop-while filter fnil get-in interleave into juxt keep map map-indexed mapcat mapv max merge min next partial rand remove repeatedly rest select-keys some take take-while update-in vals", xs)
    for (i in xs) blocked[xs[i]] = 1
    blocked_reason["apply"] = "variadic-apply-requires-dependent-fixed-arguments-and-final-sequence-expansion"
    split("assoc-in get-in update-in", xs)
    for (i in xs) blocked_reason[xs[i]] = "nested-map-paths-require-dependent-key-and-value-types"
    split("comp fnil juxt partial", xs)
    for (i in xs) blocked_reason[xs[i]] = "returned-variadic-or-overloaded-function-types-are-not-source-expressible"
    split("concat interleave map mapv", xs)
    for (i in xs) blocked_reason[xs[i]] = "variadic-multi-collection-arities-and-lazy-or-transducer-cases-are-not-source-expressible"
    split("drop drop-while filter keep map-indexed mapcat remove repeatedly take take-while", xs)
    for (i in xs) blocked_reason[xs[i]] = "upstream-lazy-sequence-or-transducer-behavior-is-not-source-expressible"
    split("doall", xs)
    for (i in xs) blocked_reason[xs[i]] = "sequence-realization-and-effect-order-remain-a-compiler-runtime-boundary"
    blocked_reason["into"] = "target-collection-representation-and-transducer-overload-require-dependent-types"
    split("max min", xs)
    for (i in xs) blocked_reason[xs[i]] = "variadic-comparable-types-and-key-callback-overloads-are-not-source-expressible"
    blocked_reason["merge"] = "variadic-map-and-record-shape-unification-is-not-source-expressible"
    blocked_reason["rand"] = "same-arity-int-and-float-bound-overloads-cannot-share-one-source-function-type"
    split("next rest", xs)
    for (i in xs) blocked_reason[xs[i]] = "nil-versus-empty-sequence-semantics-remain-a-collection-capability-boundary"
    blocked_reason["select-keys"] = "map-or-record-key-projection-requires-a-dependent-result-shape"
    blocked_reason["some"] = "nullable-first-truthy-result-needs-a-generic-witness-through-the-sequence-loop"
    blocked_reason["vals"] = "map-and-structural-record-value-projection-needs-a-closed-value-sum"
    blocked["re-find"] = 1
    blocked["re-matches"] = 1
    blocked_reason["re-find"] = "capture-count-dependent-optional-string-or-heterogeneous-capture-vector-result"
    blocked_reason["re-matches"] = "capture-count-dependent-optional-string-or-heterogeneous-capture-vector-result"
    split("clj->js clojure.pprint/pprint current-time-millis enable-console-print! ex-info future-call pr pr-sequential-writer pr-str pr-writer print println prn raise requiring-resolve resolve uuid weak-clear! weak-deref weak-ref", xs)
    for (i in xs) host[xs[i]] = 1
    split("+ - * / < <= = == > >= inc dec __lg_int __lg_long __lg_double quot rem mod bit-and bit-or bit-xor bit-not bit-shift-left bit-shift-right", xs)
    for (i in xs) primitive[xs[i]] = 1
    split("__lg_nullable-value __lg_symbol-value __lg_keyword-value __lg_int-value", xs)
    for (i in xs) narrowing[xs[i]] = 1
    internal_abi["__lg_ex-message"] = "static-exception-message-extraction-primitive"
    internal_abi["__lg_ex-cause"] = "static-optional-exception-cause-primitive"
    internal_abi["__lg_re-pattern"] = "validated-static-regex-construction-primitive"
    internal_abi["__lg_sort"] = "typed-stable-sequence-sort-primitive"
    internal_abi["__lg_sort-by"] = "typed-key-projection-and-stable-sequence-sort-primitive"
    internal_abi["__lg_reductions"] = "typed-reducer-arity-and-seqable-adaptation-primitive"
    internal_abi["__lg_reduce-kv"] = "typed-empty-accumulator-and-collection-inference-primitive"
    internal_abi["__lg_reduce"] = "typed-reduced-short-circuit-and-collection-specialization-primitive"
    internal_abi["__lg_unreduced"] = "typed-parameterized-reduced-payload-extraction-primitive"
    internal_abi["__lg_namespace"] = "typed-consumer-state-inamed-protocol-elaboration-primitive"
    internal_abi["__lg_builtin-name"] = "typed-built-in-keyword-and-symbol-name-extraction-primitive"
    internal_abi["__lg_builtin-keyword"] = "typed-string-keyword-symbol-and-optional-namespace-keyword-construction-primitive"
    internal_abi["__lg_builtin-symbol"] = "typed-string-keyword-symbol-and-optional-namespace-symbol-construction-primitive"
    internal_abi["__lg_builtin-namespace"] = "typed-built-in-keyword-and-symbol-namespace-extraction-primitive"
    internal_abi["__lg_write"] = "typed-writer-buffer-effect-primitive"
    internal_abi["__lg_assoc"] = "typed-associated-map-vector-and-record-shape-primitive"
    internal_abi["__lg_dissoc"] = "typed-map-and-record-shape-removal-primitive"
    internal_abi["__lg_contains"] = "typed-key-index-and-membership-capability-primitive"
    internal_abi["__lg_keys"] = "typed-map-key-projection-primitive"
    internal_abi["__lg_subvec"] = "typed-vector-slice-primitive"
    internal_abi["__lg_array"] = "typed-homogeneous-array-construction-primitive"
    internal_abi["__lg_hash"] = "typed-hashable-capability-primitive"
    internal_abi["__lg_compare"] = "typed-single-domain-comparable-capability-primitive"
    internal_abi["__lg_make-array"] = "typed-homogeneous-array-allocation-primitive"
    internal_abi["__lg_aget"] = "typed-array-index-capability-read-primitive"
    internal_abi["__lg_aset"] = "typed-array-index-capability-write-primitive"
    internal_abi["__lg_atom"] = "typed-reference-allocation-primitive"
    internal_abi["__lg_swap!"] = "typed-contextual-reference-swap-primitive"
    internal_abi["__lg_volatile!"] = "typed-volatile-reference-allocation-primitive"
    internal_abi["__lg_weak-deref"] = "typed-weak-reference-read-primitive"
    internal_abi["__lg_weak-clear!"] = "typed-weak-reference-clear-primitive"
    split("__lg_nil-predicate __lg_true-predicate __lg_false-predicate __lg_int-predicate __lg_number-predicate __lg_string-predicate __lg_keyword-predicate __lg_symbol-predicate __lg_list-predicate __lg_seq-predicate __lg_fn-predicate __lg_rational-predicate __lg_float-predicate __lg_double-predicate __lg_zero-predicate __lg_pos-predicate __lg_neg-predicate __lg_char-predicate __lg_identical-predicate __lg_array-predicate __lg_array-value-predicate __lg_reduced-predicate __lg_uuid-predicate __lg_delay-predicate", xs)
    for (i in xs) type_predicate[xs[i]] = 1
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
    } else if (narrowing[$0]) {
      classification = "typed-primitive"
      reason = "static-guard-narrowing-primitive"
    } else if (type_predicate[$0]) {
      classification = "typed-primitive"
      reason = "internal-static-type-predicate-primitive"
    } else if ($0 in internal_abi) {
      classification = "typed-primitive"
      reason = internal_abi[$0]
    } else if (primitive[$0]) {
      classification = "typed-primitive"
      reason = "static-scalar-primitive"
    } else if (host[$0] || $0 ~ /^\./ || $0 ~ /^js\// || $0 ~ /^__/ || $0 ~ /^-/) {
      classification = "host-boundary"
      reason = "host-interop-or-runtime-effect-boundary"
    }
    print "compiler-call\t" $0 "\t" classification "\t" reason
  }
' "$tmp/compiler-calls" >"$tmp/compiler-status"

cat "$tmp/compiler-status"

# Public forms can also be dispatched before call elaboration. Extract only
# symbols in the head position of FList patterns so generated forms and nested
# pattern syntax do not masquerade as public compiler routes.
ocaml -I +compiler-libs ocamlcommon.cma \
  "$lg_root/script/extract_ocaml_form_symbol_patterns.ml" \
  "$expression_elaborator" "$type_inference" \
  >"$tmp/compiler-forms"

form_dispatch_count=$(wc -l <"$tmp/compiler-forms" | tr -d ' ')
if test "$form_dispatch_count" -ne 129; then
  echo "compiler form dispatch changed: expected 129 names, found $form_dispatch_count" >&2
  echo "review and classify every added or removed form before updating the count" >&2
  exit 1
fi

awk -F '\t' '
  FNR == NR {
    if ($1 == "compiler-call") {
      call_status[$2] = $3
      call_reason[$2] = $4
    }
    next
  }
  {
    name = $0
    canonical = name
    sub(/^(clojure|cljs)\.core\//, "", canonical)
    status = "typed-primitive"
    reason = "static-form-elaboration-or-compiler-internal-form"
    if (canonical == "case" || canonical == "condp") {
      status = "special-form"
      reason = "compiler-owned-source-control-flow-expansion"
    } else if (canonical == "doseq") {
      status = "blocked-static-typing"
      reason = "current-effect-loop-expansion-cannot-preserve-upstream-while-early-termination-after-prior-let-modifiers"
    } else if (canonical == "for") {
      status = "special-form"
      reason = "compiler-owned-binding-modifier-and-lazy-sequence-expansion"
    } else if (canonical == "dotimes") {
      status = "special-form"
      reason = "compiler-owned-bounded-loop-expansion"
    } else if (canonical ~ /^(catch|do|if|let|let\*|loop|recur|fn|quote|try|syntax-quote|match|let-some)$/) {
      status = "special-form"
      reason = "compiler-owned-syntax-or-control-flow"
    } else if (canonical in call_status) {
      status = call_status[canonical]
      reason = call_reason[canonical]
    }
    print "compiler-form\t" name "\t" status "\t" reason
  }
' "$tmp/compiler-status" "$tmp/compiler-forms" >"$tmp/compiler-form-status"

cat "$tmp/compiler-form-status"

bb "$lg_root/script/extract_stdlib_manifest_status.clj" \
  "$lg_root/stdlib/upstream.edn" >"$tmp/manifest-status"

if test -n "$clojurescript_root"; then
  : >"$tmp/upstream-vars"
  bb "$lg_root/script/extract_clojurescript_public_vars.clj" cljs.core \
    "$clojurescript_root/src/main/cljs/cljs/core.cljs" \
    "$clojurescript_root/src/main/clojure/cljs/core.cljc" \
    >>"$tmp/upstream-vars"
  for namespace_and_source in \
    'clojure.string|src/main/cljs/clojure/string.cljs' \
    'clojure.core.protocols|src/main/cljs/clojure/core/protocols.cljs' \
    'clojure.set|src/main/cljs/clojure/set.cljs' \
    'clojure.data|src/main/cljs/clojure/data.cljs' \
    'clojure.walk|src/main/cljs/clojure/walk.cljs' \
    'clojure.edn|src/main/cljs/clojure/edn.cljs' \
    'cljs.reader|src/main/cljs/cljs/reader.cljs' \
    'cljs.math|src/main/cljs/cljs/math.cljs' \
    'cljs.pprint|src/main/cljs/cljs/pprint.cljs' \
    'cljs.test|src/main/cljs/cljs/test.cljs' \
    'cljs.spec.alpha|src/main/cljs/cljs/spec/alpha.cljs' \
    'clojure.zip|src/main/cljs/clojure/zip.cljs'; do
    namespace=${namespace_and_source%%|*}
    source=${namespace_and_source#*|}
    bb "$lg_root/script/extract_clojurescript_public_vars.clj" "$namespace" \
      "$clojurescript_root/$source" >>"$tmp/upstream-vars"
  done
  bb "$lg_root/script/extract_clojurescript_public_vars.clj" cljs.pprint \
    "$clojurescript_root/src/main/cljs/cljs/pprint.cljc" \
    >>"$tmp/upstream-vars"
  bb "$lg_root/script/extract_clojurescript_public_vars.clj" cljs.test \
    "$clojurescript_root/src/main/cljs/cljs/test.cljc" \
    >>"$tmp/upstream-vars"
  LC_ALL=C sort -u "$tmp/upstream-vars" -o "$tmp/upstream-vars"
  upstream_var_count=$(wc -l <"$tmp/upstream-vars" | tr -d ' ')
  if test "$upstream_var_count" -ne 985; then
    echo "ClojureScript public function/macro surface changed: expected 985 entries, found $upstream_var_count" >&2
    echo "review the pinned upstream files and classifications before updating the count" >&2
    exit 1
  fi

  : >"$tmp/source-vars"
  bb "$lg_root/script/extract_clojurescript_public_vars.clj" cljs.core \
    "$lg_root/stdlib/clojure/core.cljc" >>"$tmp/source-vars"
  for namespace_and_source in \
    'clojure.string|stdlib/clojure/string.cljc' \
    'clojure.core.protocols|stdlib/clojure/core/protocols.cljc' \
    'clojure.set|stdlib/clojure/set.cljc' \
    'clojure.edn|stdlib/clojure/edn.cljc' \
    'cljs.reader|stdlib/cljs/reader.cljc' \
    'cljs.math|stdlib/cljs/math.cljc' \
    'clojure.data|stdlib/clojure/data.cljc' \
    'clojure.walk|stdlib/clojure/walk.cljc' \
    'clojure.zip|stdlib/clojure/zip.cljc'; do
    namespace=${namespace_and_source%%|*}
    source=${namespace_and_source#*|}
    bb "$lg_root/script/extract_clojurescript_public_vars.clj" "$namespace" \
      "$lg_root/$source" >>"$tmp/source-vars"
  done
  LC_ALL=C sort -u "$tmp/source-vars" -o "$tmp/source-vars"

  awk -F '\t' '
    BEGIN {
      split("seq first rest next some conj", names, " ")
      for (i in names) source_inference[names[i]] = 1
    }
    FILENAME == ARGV[1] && $1 == "compiler-call" {
      compiler_call[$2] = 1
      next
    }
    FILENAME == ARGV[2] && $1 == "compiler-form" {
      name = $2
      sub(/^(clojure|cljs)\.core\//, "", name)
      compiler_form[name] = 1
      next
    }
    FILENAME == ARGV[3] && $1 ~ /^cljs\.core\// {
      name = $1
      sub(/^cljs\.core\//, "", name)
      if ((name in compiler_call) ||
          ((name in compiler_form) && !(name in source_inference))) {
        print "source core var still has name-based compiler dispatch: " name > "/dev/stderr"
        failed = 1
      }
    }
    END {exit failed}
  ' "$tmp/compiler-status" "$tmp/compiler-form-status" "$tmp/source-vars"

  awk -F '\t' '{print "source-var\t" $1 "\t" $2}' \
    "$tmp/source-vars" >"$tmp/upstream-status-input"
  cat "$tmp/compiler-status" "$tmp/compiler-form-status" \
    "$tmp/manifest-status" \
    >>"$tmp/upstream-status-input"
  awk -F '\t' '{print "upstream-var\t" $1 "\t" $2}' \
    "$tmp/upstream-vars" >>"$tmp/upstream-status-input"

  awk -F '\t' '
    $1 == "source-var" {
      source[$2 SUBSEP $3] = 1
      next
    }
    $1 == "compiler-call" {
      compiler_status[$2] = $3
      compiler_reason[$2] = $4
      next
    }
    $1 == "compiler-form" {
      name = $2
      sub(/^(clojure|cljs)\.core\//, "", name)
      form_status[name] = $3
      form_reason[name] = $4
      next
    }
    $1 == "definition" {
      definition_status[$2] = $3
      definition_reason[$2] = $4
      if (index($2, "clojure.core/") == 1) {
        core_alias = "cljs.core/" substr($2, length("clojure.core/") + 1)
        definition_status[core_alias] = $3
        definition_reason[core_alias] = $4
      }
      next
    }
    $1 == "namespace" {
      namespace_status[$2] = $3
      namespace_reason[$2] = $4
      next
    }
    $1 == "upstream-var" {
      qualified = $2
      kind = $3
      split(qualified, parts, "/")
      namespace = parts[1]
      name = substr(qualified, length(namespace) + 2)
      status = "deferred"
      reason = "not-yet-ported-or-statically-classified"
      if ((qualified SUBSEP kind) in source) {
        status = "source"
        reason = "precompiled-lg-source"
      } else if (qualified in definition_status) {
        status = definition_status[qualified]
        reason = definition_reason[qualified]
      } else if (namespace == "cljs.core" && name in compiler_status) {
        status = compiler_status[name]
        reason = compiler_reason[name]
      } else if (namespace == "cljs.core" && name in form_status) {
        status = form_status[name]
        reason = form_reason[name]
      } else if (namespace in namespace_status &&
                 (namespace_status[namespace] == "blocked-static-typing" ||
                  namespace_status[namespace] == "host-boundary" ||
                  namespace_status[namespace] == "out-of-scope")) {
        status = namespace_status[namespace]
        reason = namespace_reason[namespace]
      }
      print "upstream-var\t" qualified "\t" kind "\t" status "\t" reason
    }
  ' "$tmp/upstream-status-input" \
    | LC_ALL=C sort -t '	' -k2,2 -k3,3
fi

for namespace in \
  clojure.core cljs.core clojure.data clojure.edn cljs.reader clojure.string \
  clojure.set clojure.walk cljs.pprint cljs.test cljs.spec.alpha clojure.zip \
  clojure.test clojure.spec.alpha clojure.pprint; do
  ownership=manifest-only
  if test "$namespace" = clojure.core || test "$namespace" = cljs.core; then
    ownership=compiler-owned
  elif test "$namespace" = clojure.string \
    || test "$namespace" = clojure.edn \
    || test "$namespace" = cljs.reader \
    || test "$namespace" = clojure.data \
    || test "$namespace" = clojure.walk \
    || test "$namespace" = clojure.zip; then
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
clojure.data/diff|source
clojure.edn/read-string|source
clojure.edn/register-tag-parser!|host-boundary
cljs.reader/read-string|source
cljs.reader/register-tag-parser!|host-boundary
clojure.string/escape|source
clojure.string/split|source
clojure.walk/walk|source
clojure.walk/prewalk|source
clojure.walk/postwalk|source
clojure.walk/keywordize-keys|source
clojure.walk/stringify-keys|source
clojure.walk/prewalk-replace|source
clojure.walk/postwalk-replace|source
clojure.zip/zipper|source
clojure.zip/root|source
clojure.zip/next|source
EOF

stdlib_sources=$(rg --files "$lg_root/stdlib" -g '*.cljc')
sed -n \
  's/.*\[ocaml\.\(Lg_runtime\.Runtime_[A-Za-z0-9_]*\) :as \([A-Za-z0-9_-]*\)\].*/\1\	\2/p' \
  $stdlib_sources >"$tmp/stdlib-runtime-aliases"

(
  grep -rhoE 'Lg_runtime\.Runtime_[A-Za-z0-9_]+(\.[a-z][A-Za-z0-9_]*)+' \
    "$lg_root/src" "$lg_root/stdlib"
  while IFS="$(printf '\t')" read -r module alias; do
    rg -o --no-filename "${alias}/[a-z][A-Za-z0-9_!?-]*" \
      $stdlib_sources \
      | awk -v module="$module" '{
          member = $0
          sub(/^[^\/]*\//, "", member)
          gsub(/-/, "_", member)
          gsub(/\?/, "_question", member)
          gsub(/!/, "_bang", member)
          print module "." member
        }'
  done <"$tmp/stdlib-runtime-aliases"
) | LC_ALL=C sort -u \
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

  # Exact non-source definition classifications override their namespace. A
  # blocked or host-only sibling must never downgrade an aggregate namespace or
  # another source definition in that namespace.
  awk -F '\t' '
    $1 == "namespace" && $3 != "source-aggregate" {
      print $2 "\t" $3 "\t" $4
    }
    $1 == "definition" && $3 != "source" {
      print $2 "\t" $3 "\t" $4
    }
  ' "$tmp/manifest-status" >>"$tmp/namespace-support"

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
          ($2 in support ? support[$2] : namespace_status(namespace)) "\t" $3 "\t" \
          ($2 in reason ? reason[$2] : namespace_reason(namespace))
      }
    }
  ' "$tmp/namespace-support" "$tmp/logseq-counts" \
    | LC_ALL=C sort -t '	' -k1,1 -k2,2
fi
