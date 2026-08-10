#!/bin/sh
set -eu

root=$1

for name in identity completing complement boolean some? boolean? empty? not-empty integer? ident? counted? seqable? nat-int? pos-int? neg-int? simple-symbol? qualified-symbol? simple-keyword? qualified-keyword? simple-ident? qualified-ident? reduced ensure-reduced reset-vals! subs int-to-string-radix any? range shuffle inc dec bit-not int-rotate-left imul m3-mix-K1 m3-mix-H1 m3-fmix m3-hash-int m3-hash-unencoded-chars hash-string* add-to-string-hash-cache mix-collection-hash ratio? decimal? realized? alength aclone acopy aslice aconcat array-values array-to-seq array-to-rseq array-seq to-array into-array array-from array-index-of array-binary-search-left array-binary-search-right amap asort! quot rem mod unchecked-add unchecked-add-int unchecked-subtract unchecked-subtract-int unchecked-multiply unchecked-multiply-int unchecked-divide-int unchecked-remainder-int unchecked-inc unchecked-inc-int unchecked-dec unchecked-dec-int unchecked-negate unchecked-negate-int unchecked-int unchecked-long rand-int rand-nth bit-and bit-or bit-xor bit-shift-left bit-shift-right bit-shift-right-zero-fill bit-and-not unsigned-bit-shift-right bit-count second last even? odd? every? ffirst fnext nfirst nnext not-any? not-every? split-at split-with nthnext nthrest bounded-count butlast take-last drop-last reverse interpose dedupe distinct distinct? not= zipmap comparator frequencies update-vals update-keys hash-combine max-key min-key constantly vec iterate tree-seq partitionv flush random-uuid parse-uuid system-time parse-long parse-double merge-with NaN? infinite? keyword-identical? symbol-identical? hash-double hash-keyword hash-string hash-long hash hash-ordered-coll hash-unordered-coll compare make-array aget aset atom volatile! transient persistent! conj! assoc! dissoc! pop! disj! get get-in assoc-in update update-in select-keys merge vals special-symbol? meta with-meta ifind? map-entry? regexp? volatile? key-test equiv-map divide reduceable? vector-lite hash-map-lite set-lite; do
  for file in \
    src/call_elaborator.ml \
    src/core_collection.ml \
    src/expression_support.ml \
    src/core_boolean.ml \
    src/core_predicate.ml \
    src/core_scalar.ml \
    src/function_combinator_elaborator.ml \
    src/sequence_call_elaborator.ml \
    src/special_form_elaborator.ml \
    src/type_inference.ml; do
    # First-class diagnostics are static boundaries, not implementations.
    if test "$file" = src/expression_support.ml; then
      case "$name" in
        nil\?|true\?|false\?|number\?|string\?|keyword\?|symbol\?|vec)
          continue
          ;;
      esac
    fi
    if grep -F "\"$name\"" "$root/$file" >/dev/null; then
      echo "clojure.core/$name is still compiler-dispatched in $file" >&2
      exit 1
    fi
  done
done

for name in seq first rest next cons; do
  if grep -E "^[[:space:]]*\| .*\"$name\".*->" \
      "$root/src/call_elaborator.ml" "$root/src/core_collection.ml" >/dev/null; then
    echo "clojure.core/$name is still publicly dispatched by the collection compiler" >&2
    exit 1
  fi
done

for name in get get-in assoc-in update update-in select-keys merge vals; do
  for file in \
    src/collection_operation_elaborator.ml \
    src/core_form_expansion.ml \
    src/expression_elaborator.ml; do
    if grep -F "\"$name\"" "$root/$file" >/dev/null; then
      echo "clojure.core/$name is still internally expanded by its public name in $file" >&2
      exit 1
    fi
  done
done

public_predicates='nil? true? false? int? number? string? keyword? symbol? vector? list? seq? set? map? fn? coll? associative? rational? float? double? sequential? reversible? sorted? zero? pos? neg? abs char? identical? array? array-value? reduced?'
dispatch_names=$(ocaml -I +compiler-libs ocamlcommon.cma \
  "$root/script/extract_ocaml_string_dispatch.ml" \
  "$root/src/call_elaborator.ml")
for name in $public_predicates; do
  if printf '%s\n' "$dispatch_names" | grep -Fx "$name" >/dev/null; then
    echo "clojure.core/$name is still publicly dispatched in call_elaborator.ml" >&2
    exit 1
  fi
  if grep -E "^[[:space:]]*\\| \"$name\"([[:space:]]|->|\\|)" \
    "$root/src/core_boolean.ml" "$root/src/core_predicate.ml" >/dev/null; then
    echo "clojure.core/$name is still implemented by a public compiler predicate" >&2
    exit 1
  fi
done

public_source_primitives='int long double byte float short unchecked-byte unchecked-char unchecked-short unchecked-float unchecked-double'
for name in $public_source_primitives; do
  if printf '%s\n' "$dispatch_names" | grep -Fx "$name" >/dev/null; then
    echo "clojure.core/$name is still publicly dispatched in call_elaborator.ml" >&2
    exit 1
  fi
  if grep -F "FSymbol \"$name\"" "$root/src/type_inference.ml" >/dev/null; then
    echo "clojure.core/$name is still publicly inferred in type_inference.ml" >&2
    exit 1
  fi
done

if printf '%s\n' "$dispatch_names" | grep -Fx 'ex-message' >/dev/null \
  || grep -F 'FSymbol "ex-message"' "$root/src/type_inference.ml" >/dev/null; then
  echo "clojure.core/ex-message still has public-name compiler ownership" >&2
  exit 1
fi

if printf '%s\n' "$dispatch_names" | grep -Fx 'ex-cause' >/dev/null \
  || grep -F 'FSymbol "ex-cause"' "$root/src/type_inference.ml" >/dev/null; then
  echo "clojure.core/ex-cause still has public-name compiler ownership" >&2
  exit 1
fi

if printf '%s\n' "$dispatch_names" | grep -Fx 're-pattern' >/dev/null \
  || grep -F 'FSymbol "re-pattern"' "$root/src/type_inference.ml" >/dev/null; then
  echo "clojure.core/re-pattern still has public-name compiler ownership" >&2
  exit 1
fi

if printf '%s\n' "$dispatch_names" | grep -Fx 'gensym' >/dev/null \
  || grep -F 'FSymbol "gensym"' "$root/src/type_inference.ml" >/dev/null; then
  echo "clojure.core/gensym still has runtime public-name compiler ownership" >&2
  exit 1
fi

if printf '%s\n' "$dispatch_names" | grep -Fx 'truth_' >/dev/null \
  || grep -F 'FSymbol "truth_"' "$root/src/type_inference.ml" >/dev/null; then
  echo "clojure.core/truth_ still has runtime public-name compiler ownership" >&2
  exit 1
fi

for name in comment doto when-first while if-not when when-not cond unchecked-max unchecked-min mask bitpos caching-hash areduce locking '->' '->>' 'as->' 'cond->' 'cond->>' 'some->' 'some->>'; do
  for file in src/call_elaborator.ml src/expression_elaborator.ml \
    src/macro_expander.ml src/special_form_elaborator.ml src/top_level_elaborator.ml \
    src/type_inference.ml; do
    matches=$(grep -F "\"$name\"" "$root/$file" || true)
    if test "$name" = when && test "$file" = src/special_form_elaborator.ml; then
      matches=$(printf '%s\n' "$matches" \
        | grep -v 'pattern_form; guard_form' || true)
    fi
    if test -n "$matches"; then
      echo "clojure.core/$name is still compiler-owned in $file" >&2
      exit 1
    fi
  done
done

if grep -F 'FList (FSymbol "and" :: forms) -> compile_logical' \
    "$root/src/expression_elaborator.ml" >/dev/null \
  || grep -F 'FList (FSymbol "or" :: forms) -> compile_logical' \
    "$root/src/expression_elaborator.ml" >/dev/null \
  || grep -F 'FSymbol ("and" | "clojure.core/and")' \
    "$root/src/macro_expander.ml" >/dev/null \
  || grep -F 'FSymbol "or" :: forms' \
    "$root/src/macro_expander.ml" >/dev/null \
  || grep -F 'FList (FSymbol ("and" | "or") :: forms)' \
    "$root/src/type_inference.ml" >/dev/null \
  || grep -F 'FList (FSymbol "and" :: conditions)' \
    "$root/src/type_inference.ml" >/dev/null \
  || grep -F 'FList (FSymbol "or" :: conditions)' \
    "$root/src/type_inference.ml" >/dev/null; then
  echo "clojure.core/and or clojure.core/or still has public-name compiler ownership" >&2
  exit 1
fi

for name in if-let when-let if-some when-some; do
  if grep -F "FSymbol \"$name\"" "$root/src/expression_elaborator.ml" \
      "$root/src/macro_expander.ml" >/dev/null \
    || grep -F "FSymbol (\"$name\"" "$root/src/expression_elaborator.ml" \
      "$root/src/special_form_elaborator.ml" >/dev/null; then
    echo "clojure.core/$name still has public-name compiler ownership" >&2
    exit 1
  fi
done

if grep -F 'FSymbol "not"' "$root/src/type_inference.ml" >/dev/null \
  || grep -F '"not" | "nil?"' "$root/src/call_elaborator.ml" >/dev/null \
  || grep -F '| "not" -> compile_not' "$root/src/core_boolean.ml" >/dev/null; then
  echo "clojure.core/not still has public-name compiler dispatch" >&2
  exit 1
fi
