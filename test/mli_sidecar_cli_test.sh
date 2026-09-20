#!/bin/sh
set -eu
cli="$1"
stdlib_state="$2"
directory=$(mktemp -d "${TMPDIR:-/tmp}/lg-mli-sidecar.XXXXXX")
trap 'rm -rf "$directory"' EXIT HUP INT TERM

cat > "$directory/sample.mli" <<'EOF'
type person = { age : int }
val next_age : person -> int
EOF
cat > "$directory/sample.cljc" <<'EOF'
(ns sample)
(defn next-age [person] (+ (:age person) 1))
EOF
# An unrelated host interface must not be treated as an LG sidecar.
printf 'module type Host = sig end\n' > "$directory/host.mli"
"$cli" --compile-files-from "$stdlib_state" "$directory" -o "$directory/directory.ml"
"$cli" --compile-files-from "$stdlib_state" "$directory/sample.cljc" \
  "$directory/sample.mli" -o "$directory/explicit.ml"
cmp "$directory/directory.ml" "$directory/explicit.ml"
"$cli" --compile-files-from "$stdlib_state" "$directory/sample.cljc" -o "$directory/auto.ml"
cmp "$directory/directory.ml" "$directory/auto.ml"
"$cli" --compile-chunk-from "$stdlib_state" "$directory/sample.cljc" -o "$directory/chunk.ml"

"$cli" --compile-chunk-state "$stdlib_state" "$directory/pending.state" \
  "$directory/sample.mli" -o "$directory/pending.ml"
"$cli" --compile-chunk-from "$directory/pending.state" "$directory/sample.cljc" \
  -o "$directory/resumed.ml"

cat > "$directory/sample.cljc" <<'EOF'
(ns sample)
(defn next-age [person] "wrong")
EOF
if "$cli" --compile-chunk-state "$directory/pending.state" "$directory/invalid.state" \
  "$directory/sample.cljc" -o "$directory/invalid.ml" 2> "$directory/error"; then
  echo 'mli contract accepted an invalid saved-state continuation' >&2
  exit 1
fi
test ! -e "$directory/invalid.state"
test ! -e "$directory/invalid.ml"
if ! grep -Eqi 'type|expected|incompatible|cannot adapt' "$directory/error"; then
  cat "$directory/error" >&2
  exit 1
fi

mkdir "$directory/plain"
printf 'val answer : unit -> int\n' > "$directory/plain/main.mli"
printf '(defn answer [] 42)\n' > "$directory/plain/main.cljc"
"$cli" --compile-files "$directory/plain/main.cljc" "$directory/plain/main.mli" -o "$directory/plain.ml"
"$cli" "$directory/plain/main.cljc" -o "$directory/single.ml"
printf '(defn answer [] "wrong")\n' > "$directory/plain/main.cljc"
if "$cli" "$directory/plain/main.cljc" -o "$directory/bad-single.ml" 2> "$directory/error"; then
  echo 'single-file compilation ignored its adjacent interface' >&2
  exit 1
fi
test ! -e "$directory/bad-single.ml"

if "$cli" --interface "$directory/plain/main.cljc" -o "$directory/bad-interface.txt" 2> "$directory/error"; then
  echo 'interface inference ignored the declared contract' >&2
  exit 1
fi
test ! -e "$directory/bad-interface.txt"
