#!/bin/sh

set -eu

root=$1
actual=$(mktemp "${TMPDIR:-/tmp}/lg-core-boundaries.XXXXXX")
trap 'rm -f "$actual"' EXIT HUP INT TERM

bb "$root/script/extract_stdlib_manifest_status.clj" \
  "$root/stdlib/upstream.edn" >"$actual"

tab=$(printf '\t')
failed=0
while IFS="$tab" read -r name status reason; do
  if ! awk -F '\t' -v name="clojure.core/$name" -v status="$status" \
      -v reason="$reason" \
      '$1 == "definition" && $2 == name && $3 == status && $4 == reason { found=1 }
       END { exit !found }' "$actual"; then
    echo "clojure.core/$name is missing audited $status reason: $reason" >&2
    failed=1
  fi
done <<'EOF'
clone	source	source-function-dispatches-through-icloneable-and-preserves-equal-values-with-fresh-nonempty-list-vector-sequence-and-hash-map-identity-while-ocaml-empty-list-and-vector-singletons-retain-identity
cloneable?	source	source-function-and-inline-specialization-use-an-optional-static-icloneable-witness-so-first-class-true-and-false-calls-require-no-open-value-or-dynamic-dispatch
chunked-seq?	source	source-function-and-inline-specialization-use-an-optional-static-ichunkedseq-witness-so-first-class-true-and-false-calls-require-no-open-value-or-dynamic-dispatch
default-dispatch-val	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
delay?	source	source-public-function-matches-cljs-delay-instance-predicate-with-a-first-class-lazy-signature-and-inline-static-specialization-for-arbitrary-static-values
doseq	source	source-macro-preserves-clojurescript-binding-modifier-order-and-per-loop-while-termination-without-the-javascript-chunked-sequence-fast-path
ensure-reduced	source	source-public-function-matches-cljs-conditional-reduced-wrapper-with-a-first-class-non-reduced-signature-and-inline-static-specialization-that-preserves-an-existing-parameterized-wrapper
force	source	source-public-function-matches-cljs-delay-force-with-a-first-class-lazy-signature-and-inline-static-specialization-that-preserves-non-delay-input-types
ifn?	source	source-function-uses-a-strict-function-fallback-signature-while-inline-specialization-delegates-to-a-private-static-callable-type-predicate-covering-functions-ifn-deftypes-keywords-symbols-vectors-maps-and-sets-without-runtime-dynamic-dispatch
implements?	source	source-macro-preserves-static-protocol-satisfaction-and-single-value-evaluation-by-expanding-to-satisfies-while-clojurescript-javascript-mask-layout-is-not-part-of-lg-runtime-representation
list*	source	source-macro-preserves-all-direct-upstream-arities-final-sequence-expansion-left-to-right-single-evaluation-and-existing-eager-typed-list-results-for-one-static-element-type-while-the-complete-first-class-heterogeneous-variadic-function-shape-remains-unrepresentable
rand	source	source-public-overloads-preserve-cljs-zero-and-one-arity-floating-results-with-inline-static-int-or-float-bound-specialization
record?	source	source-function-and-inline-specialization-use-an-optional-static-irecord-marker-witness-with-implicit-satisfaction-restricted-to-defrecord-values-and-no-open-value-or-dynamic-dispatch
replace	source	source-port-preserves-the-upstream-transducer-arity-and-shape-dependent-vector-or-lazy-sequence-results-through-inline-static-protocol-dispatch-while-vector-metadata-remains-unavailable-on-the-current-static-vector-representation
remove-all-methods	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
remove-method	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
spread	blocked-static-typing	argument-list-elements-are-heterogeneous-because-only-the-final-element-is-expanded-as-a-sequence
tagged-literal	source	source-parameterized-nominal-type-preserves-symbol-tags-statically-typed-payloads-literal-keyword-or-get-field-access-structural-equality-and-upstream-hash-composition-without-an-open-dynamic-value
tagged-literal?	source	source-function-and-inline-specialization-use-an-optional-static-itaggedliteral-marker-witness-for-first-class-true-and-false-calls
trampoline	blocked-static-typing	step-results-recursively-alternate-between-zero-arity-functions-and-final-values-and-the-second-arity-also-requires-variadic-apply
to-array-2d	source	source-port-preserves-ragged-nested-seqable-conversion-through-static-inner-and-outer-sequence-witnesses
unsafe-bit-and	blocked-static-typing	javascript-result-is-numeric-but-analyzer-boolean-context-uses-zero-falsiness-which-one-static-source-type-cannot-preserve
uuid?	source	source-public-function-matches-cljs-iuuid-predicate-with-a-first-class-nominal-uuid-signature-and-inline-static-specialization-that-distinguishes-ordinary-strings
vec-lite	blocked-static-typing	one-arity-result-depends-on-map-entry-vector-array-or-general-seqable-input-representation
coercive-=	host-boundary	javascript-loose-equality-crosses-static-type-domains-and-has-no-portable-native-equivalent
coercive-boolean	host-boundary	javascript-falsiness-for-zero-nan-empty-string-null-and-undefined-differs-from-clojure-truthiness
coercive-not	host-boundary	javascript-falsiness-for-zero-nan-empty-string-null-and-undefined-differs-from-clojure-truthiness
coercive-not=	host-boundary	javascript-loose-inequality-crosses-static-type-domains-and-has-no-portable-native-equivalent
inst-ms	host-boundary	clojurescript-inst-is-a-javascript-date-protocol-with-no-shared-native-source-representation
inst?	host-boundary	clojurescript-inst-is-a-javascript-date-protocol-with-no-shared-native-source-representation
js-symbol?	host-boundary	javascript-symbol-is-a-target-specific-nominal-host-type
newline	source	source-zero-and-nil-options-arities-write-the-upstream-newline-through-the-cross-target-static-output-boundary-while-open-print-function-and-flush-options-remain-a-host-boundary
object?	host-boundary	tests-the-javascript-object-constructor-and-has-no-equivalent-native-object-category
unsafe-cast	host-boundary	emits-a-javascript-closure-type-cast-and-assignment-for-the-analyzer
uri?	host-boundary	tests-the-google-closure-uri-class-which-has-no-native-source-representation
var?	host-boundary	tests-the-clojurescript-javascript-var-wrapper-which-lg-does-not-expose-as-a-source-value
write-all	source	source-variadic-function-preserves-left-to-right-string-writes-through-the-static-iwriter-buffer-protocol
EOF

exit "$failed"
