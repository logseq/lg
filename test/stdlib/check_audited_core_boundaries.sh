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
clone	blocked-static-typing	fresh-clone-identity-is-observable-and-cannot-be-preserved-for-every-immutable-static-collection-representation
cloneable?	blocked-static-typing	first-class-protocol-predicate-must-accept-every-static-value-type-without-a-universal-open-value
default-dispatch-val	blocked-static-typing	requires-the-clojurescript-imultifn-protocol-and-multimethod-runtime-domain
ifn?	blocked-static-typing	first-class-predicate-combines-function-types-and-arbitrary-ifn-implementations-without-a-static-union-capability
record?	blocked-static-typing	first-class-marker-predicate-must-accept-arbitrary-record-and-non-record-static-types-without-a-universal-open-value
replace	blocked-static-typing	one-arity-transducer-and-two-arity-vector-or-lazy-sequence-dependent-results-cannot-share-one-source-function-type
spread	blocked-static-typing	argument-list-elements-are-heterogeneous-because-only-the-final-element-is-expanded-as-a-sequence
swap-vals!	blocked-static-typing	atomic-old-new-results-require-callback-arity-overloads-inside-the-reference-operation-rather-than-a-non-atomic-deref-and-swap-composition
tagged-literal	blocked-static-typing	tagged-literal-forms-accept-arbitrary-clojure-values-without-a-public-closed-source-value-domain
tagged-literal?	blocked-static-typing	first-class-nominal-predicate-must-accept-every-static-value-type-without-a-universal-open-value
trampoline	blocked-static-typing	step-results-recursively-alternate-between-zero-arity-functions-and-final-values-and-the-second-arity-also-requires-variadic-apply
unsafe-bit-and	blocked-static-typing	javascript-result-is-numeric-but-analyzer-boolean-context-uses-zero-falsiness-which-one-static-source-type-cannot-preserve
vary-meta	blocked-static-typing	metadata-transform-callbacks-span-five-fixed-arities-plus-variadic-apply-while-preserving-the-receiver-type
vec-lite	blocked-static-typing	one-arity-result-depends-on-map-entry-vector-array-or-general-seqable-input-representation
coercive-=	host-boundary	javascript-loose-equality-crosses-static-type-domains-and-has-no-portable-native-equivalent
coercive-boolean	host-boundary	javascript-falsiness-for-zero-nan-empty-string-null-and-undefined-differs-from-clojure-truthiness
coercive-not	host-boundary	javascript-falsiness-for-zero-nan-empty-string-null-and-undefined-differs-from-clojure-truthiness
coercive-not=	host-boundary	javascript-loose-inequality-crosses-static-type-domains-and-has-no-portable-native-equivalent
inst-ms	host-boundary	clojurescript-inst-is-a-javascript-date-protocol-with-no-shared-native-source-representation
inst?	host-boundary	clojurescript-inst-is-a-javascript-date-protocol-with-no-shared-native-source-representation
js-symbol?	host-boundary	javascript-symbol-is-a-target-specific-nominal-host-type
newline	host-boundary	uses-dynamic-print-function-and-flush-options-from-the-clojurescript-host-printing-runtime
object?	host-boundary	tests-the-javascript-object-constructor-and-has-no-equivalent-native-object-category
unsafe-cast	host-boundary	emits-a-javascript-closure-type-cast-and-assignment-for-the-analyzer
uri?	host-boundary	tests-the-google-closure-uri-class-which-has-no-native-source-representation
var?	host-boundary	tests-the-clojurescript-javascript-var-wrapper-which-lg-does-not-expose-as-a-source-value
write-all	host-boundary	writes-through-clojurescript-iwriter-and-target-printing-effects-rather-than-a-portable-pure-value
EOF

exit "$failed"
