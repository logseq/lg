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
  case "$name" in
    */*) qualified_name="$name" ;;
    *) qualified_name="clojure.core/$name" ;;
  esac
  if ! awk -F '\t' -v name="$qualified_name" -v status="$status" \
      -v reason="$reason" \
      '$1 == "definition" && $2 == name && $3 == status && $4 == reason { found=1 }
       END { exit !found }' "$actual"; then
    echo "$qualified_name is missing audited $status reason: $reason" >&2
    failed=1
  fi
done <<'EOF'
clone	source	source-function-dispatches-through-icloneable-and-preserves-equal-values-with-fresh-nonempty-list-vector-sequence-and-hash-map-identity-while-ocaml-empty-list-and-vector-singletons-retain-identity
cloneable?	source	source-function-and-inline-specialization-use-an-optional-static-icloneable-witness-so-first-class-true-and-false-calls-require-no-open-value-or-dynamic-dispatch
chunked-seq?	source	source-function-and-inline-specialization-use-an-optional-static-ichunkedseq-witness-so-first-class-true-and-false-calls-require-no-open-value-or-dynamic-dispatch
chunked-seq	typed-primitive	vector-trie-node-array-chunk-construction-is-an-internal-clojurescript-persistent-vector-representation-boundary-while-public-lg-chunk-apis-use-source-array-chunk-and-chunk-cons
*exec-tap-fn*	source	source-function-delegates-to-a-documented-runtime-tap-callback-dynamic-boundary-and-executes-synchronously-on-native-and-melange-instead-of-the-upstream-javascript-settimeout-scheduler
add-tap	source	source-function-preserves-upstream-nil-result-and-global-callback-registration-through-a-narrow-runtime-dynamic-boundary-for-open-tap-values
*print-meta*	source	source-dynamic-var-preserves-the-upstream-boolean-metadata-printing-switch-as-a-static-bool-ref-while-metadata-rendering-remains-explicitly-unsupported
*print-dup*	source	source-dynamic-var-preserves-the-upstream-boolean-duplicate-printing-switch-as-a-static-bool-ref-while-duplicate-constructor-rendering-remains-explicitly-unsupported
*print-namespace-maps*	source	source-dynamic-var-preserves-the-upstream-boolean-namespace-map-lifting-switch-as-a-static-bool-ref-while-prefix-map-rendering-remains-explicitly-unsupported
default-dispatch-val	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
delay?	source	source-public-function-matches-cljs-delay-instance-predicate-with-a-first-class-lazy-signature-and-inline-static-specialization-for-arbitrary-static-values
doseq	source	source-macro-preserves-clojurescript-binding-modifier-order-and-per-loop-while-termination-without-the-javascript-chunked-sequence-fast-path
ensure-reduced	source	source-public-function-matches-cljs-conditional-reduced-wrapper-with-a-first-class-non-reduced-signature-and-inline-static-specialization-that-preserves-an-existing-parameterized-wrapper
force	source	source-public-function-matches-cljs-delay-force-with-a-first-class-lazy-signature-and-inline-static-specialization-that-preserves-non-delay-input-types
flatten	source	source-public-wrapper-preserves-cljs-flatten-for-statically-homogeneous-seqable-layers-through-a-private-typed-flatten-primitive-without-dynamic-erasure
ifn?	source	source-function-uses-a-strict-function-fallback-signature-while-inline-specialization-delegates-to-a-private-static-callable-type-predicate-covering-functions-ifn-deftypes-keywords-symbols-vectors-maps-and-sets-without-runtime-dynamic-dispatch
implements?	source	source-macro-preserves-static-protocol-satisfaction-and-single-value-evaluation-by-expanding-to-satisfies-while-clojurescript-javascript-mask-layout-is-not-part-of-lg-runtime-representation
iteration	source	source-port-preserves-direct-upstream-keyword-option-calls-through-a-static-inline-rewrite-and-returns-a-memoized-seq-while-first-class-calls-use-the-five-argument-static-abi-instead-of-heterogeneous-keyword-varargs
list*	source	source-macro-preserves-all-direct-upstream-arities-final-sequence-expansion-left-to-right-single-evaluation-and-existing-eager-typed-list-results-for-one-static-element-type-while-the-complete-first-class-heterogeneous-variadic-function-shape-remains-unrepresentable
memoize	source	source-macro-expands-direct-calls-to-a-private-typed-memoize-abi-for-zero-through-three-fixed-arity-functions-using-static-cache-keys-while-first-class-variadic-and-heterogeneous-argument-tuple-memoize-remains-unrepresentable
prefer-method	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
prefers	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
rand	source	source-public-overloads-preserve-cljs-zero-and-one-arity-floating-results-with-inline-static-int-or-float-bound-specialization
with-redefs	source	source-macro-expands-to-a-private-typed-var-root-rebinding-form-for-ordinary-monomorphic-source-functions-that-restores-values-after-body-or-exception-and-keeps-replacement-values-at-the-root-static-type
cljs.pprint/deftype	source	source-macro-preserves-the-upstream-defrecord-constructor-and-type-tag-predicate-expansion-while-field-types-remain-inferred-by-lg-static-record-use-instead-of-dynamic-fields
cljs.test/assert-expr	source	source-macro-exposes-the-default-static-boolean-assertion-expansion-used-by-is-while-clojurescript-analyzer-time-user-defmethod-extension-remains-a-documented-macro-time-boundary
cljs.test/update-current-env!	source	source-macro-preserves-current-test-environment-updates-for-supported-literal-paths-over-the-closed-test-env-record-while-rejecting-open-runtime-paths-instead-of-erasing-the-environment-to-dynamic
persistent-array-map-seq	typed-primitive	lg-array-map-is-a-source-wrapper-over-the-static-map-primitive-and-its-seq-is-provided-by-typed-map-entry-iteration-instead-of-clojurescript-alternating-array-storage
record?	source	source-function-and-inline-specialization-use-an-optional-static-irecord-marker-witness-with-implicit-satisfaction-restricted-to-defrecord-values-and-no-open-value-or-dynamic-dispatch
replace	source	source-port-preserves-the-upstream-transducer-arity-and-shape-dependent-vector-or-lazy-sequence-results-through-inline-static-protocol-dispatch-while-vector-metadata-remains-unavailable-on-the-current-static-vector-representation
remove-all-methods	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
remove-method	source	precompiled-lg-source-macro-with-runtime-multifn-dynamic-boundary
remove-tap	source	source-function-preserves-upstream-nil-result-and-symbol-callback-removal-through-the-documented-runtime-tap-dynamic-boundary
seq-to-map-for-destructuring	special-form	compiler-owned-destructuring-helper-covered-by-static-map-and-sequential-binding-elaboration-with-closed-key-value-types-instead-of-a-first-class-heterogeneous-helper-function
spread	special-form	compiler-owned-apply-argument-splicing-helper-with-heterogeneous-fixed-arguments-and-a-typed-final-seqable-tail-covered-by-the-source-apply-wrapper-and-__lg_apply-primitive
tap>	source	source-function-preserves-upstream-boolean-result-and-open-value-delivery-through-a-narrow-runtime-dynamic-boundary-with-static-call-site-conversion
tagged-literal	source	source-parameterized-nominal-type-preserves-symbol-tags-statically-typed-payloads-literal-keyword-or-get-field-access-structural-equality-and-upstream-hash-composition-without-an-open-dynamic-value
tagged-literal?	source	source-function-and-inline-specialization-use-an-optional-static-itaggedliteral-marker-witness-for-first-class-true-and-false-calls
trampoline	source	source-function-preserves-the-upstream-zero-arity-bounce-loop-through-an-explicit-trampoline-step-closed-sum-instead-of-runtime-fn-predicate-dispatch-while-the-upstream-variadic-arity-remains-covered-by-the-existing-typed-apply-abi
to-array-2d	source	source-port-preserves-ragged-nested-seqable-conversion-through-static-inner-and-outer-sequence-witnesses
unsafe-bit-and	source	source-function-exposes-the-static-integer-bitwise-and-result-while-javascript-zero-falsiness-remains-a-separate-host-boundary
uuid?	source	source-public-function-matches-cljs-iuuid-predicate-with-a-first-class-nominal-uuid-signature-and-inline-static-specialization-that-distinguishes-ordinary-strings
vec-lite	source	source-public-wrapper-preserves-cljs-seqable-to-vector-realization-through-the-existing-static-vec-boundary
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
