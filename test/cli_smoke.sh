#!/bin/sh
set -eu

cli="$1"
example="$2"
checkout_example="$(dirname "$example")/checkout.cljc"
protocols_example="$(dirname "$example")/protocols.cljc"

output="$($cli --run "$example")"

[ "$output" = "ADA:true:1" ]

checkout_output="$($cli --run "$checkout_example")"

[ "$checkout_output" = "ADA:standard:4999
ADA:express:5399" ]

protocols_output="$($cli --run "$protocols_example")"

[ "$protocols_output" = "audit #42/#7 ADA severity=:warning enabled" ]

invalid_source="$(mktemp)"
invalid_stdout="$(mktemp)"
invalid_stderr="$(mktemp)"
warning_source="$(mktemp)"
warning_stdout="$(mktemp)"
warning_stderr="$(mktemp)"
trap 'exit_status=$?; rm -f "$invalid_source" "$invalid_stdout" "$invalid_stderr" "$warning_source" "$warning_stdout" "$warning_stderr"; exit "$exit_status"' EXIT

if "$cli" --target browser "$example" >"$invalid_stdout" 2>"$invalid_stderr"; then
  echo "expected unknown target to fail" >&2
  exit 1
fi

grep -q "lg: unknown target browser; expected native, melange, or js" "$invalid_stderr"

printf '%s\n' \
  '(def ok 1)' \
  '' \
  '(def answer (Stdlib.abs "bad"))' > "$invalid_source"

if "$cli" "$invalid_source" >"$invalid_stdout" 2>"$invalid_stderr"; then
  echo "expected invalid host call to fail" >&2
  exit 1
fi

grep -q "lg: OCaml argument type mismatch" "$invalid_stderr"
grep -q "File \"$invalid_source\", line 3" "$invalid_stderr"

printf '%s\n' \
  '(type-variant status Active Inactive)' \
  '(defn describe [^:status status]' \
  '  (match status Active "active"))' > "$warning_source"

"$cli" "$warning_source" >"$warning_stdout" 2>"$warning_stderr"

grep -q 'describe' "$warning_stdout"
grep -q 'Warning 8' "$warning_stderr"
grep -q 'not exhaustive' "$warning_stderr"
grep -q "File \"$warning_source\", line 1" "$warning_stderr"

package_source="$(mktemp)"
package_stdout="$(mktemp)"
multi_dir="$(mktemp -d)"
trap 'exit_status=$?; rm -f "$invalid_source" "$invalid_stdout" "$invalid_stderr" "$warning_source" "$warning_stdout" "$warning_stderr" "$package_source" "$package_stdout"; rm -rf "$multi_dir"; exit "$exit_status"' EXIT

printf '%s\n' \
  '(require [ocaml.package/core] [ocaml.Core.Int :as int])' \
  '(println (int/abs -42))' > "$package_source"

"$cli" --run "$package_source" > "$package_stdout"

[ "$(cat "$package_stdout")" = "42" ]

math_source="$multi_dir/math.cljc"
main_source="$multi_dir/main.cljc"
multi_output="$multi_dir/app.ml"
multi_stdout="$multi_dir/stdout"
multi_cache_stderr="$multi_dir/cache.stderr"

printf '%s\n' \
  '(require [ocaml.package/core]' \
  '         [ocaml.Core.Int :as int])' \
  '(module Math' \
  '  (defn magnitude-plus-two [x] (+ (int/abs x) 2)))' > "$math_source"

printf '%s\n' \
  '(println (Math/magnitude-plus-two -40))' > "$main_source"

LG_CACHE_DIR="$multi_dir/cache" \
  "$cli" --compile-files "$math_source" "$main_source" -o "$multi_output"
grep -q 'magnitude_plus_two' "$multi_output"

LG_CACHE_DIR="$multi_dir/cache" LG_COMPILE_CACHE_DEBUG=1 \
  "$cli" --compile-files "$math_source" "$main_source" -o "$multi_output" \
  2> "$multi_cache_stderr"
grep -q "compile cache hit: $math_source" "$multi_cache_stderr"
grep -q "compile cache hit: $main_source" "$multi_cache_stderr"

base_state="$multi_dir/base.state"
base_output="$multi_dir/base.ml"
suffix_state="$multi_dir/suffix.state"
suffix_output="$multi_dir/suffix.ml"
cached_suffix_state="$multi_dir/cached-suffix.state"
cached_suffix_output="$multi_dir/cached-suffix.ml"
suffix_cache_stderr="$multi_dir/suffix-cache.stderr"
continuation_source="$multi_dir/continuation.cljc"
continuation_output="$multi_dir/continuation.ml"
post_source="$multi_dir/post.cljc"
partial_output="$multi_dir/partial.ml"
partial_cache_stderr="$multi_dir/partial-cache.stderr"

printf '%s\n' '(def continued Math/magnitude-plus-two)' > "$continuation_source"

LG_CACHE_DIR="$multi_dir/state-cache" \
  "$cli" --compile-files-state "$base_state" "$math_source" -o "$base_output"
if [ -d "$multi_dir/state-cache/compile-files" ]; then
  echo "state-producing compilation wrote redundant prefix cache" >&2
  exit 1
fi
LG_CACHE_DIR="$multi_dir/state-cache" \
  "$cli" --compile-files-from-state "$base_state" "$suffix_state" \
    "$main_source" "$continuation_source" -o "$suffix_output"
if [ -d "$multi_dir/state-cache/compile-files" ]; then
  echo "state-producing continuation wrote redundant prefix cache" >&2
  exit 1
fi
LG_CACHE_DIR="$multi_dir/state-cache" \
  "$cli" --compile-files-from-state "$base_state" "$cached_suffix_state" \
    "$main_source" "$continuation_source" -o "$cached_suffix_output" \
    2> "$suffix_cache_stderr"
if grep -q "compile cache hit:" "$suffix_cache_stderr"; then
  echo "state-producing continuation read a prefix cache" >&2
  exit 1
fi
cmp "$suffix_output" "$cached_suffix_output"

printf '%s\n' '(def post continued)' > "$post_source"
"$cli" --compile-chunk-from "$cached_suffix_state" "$post_source" \
  -o "$continuation_output"
grep -q 'let post = continued' "$continuation_output"

LG_CACHE_DIR="$multi_dir/state-cache" \
  "$cli" --compile-files-from "$base_state" "$main_source" \
    "$continuation_source" -o "$partial_output"

printf '%s\n' '(def continued-value Math/magnitude-plus-two)' \
  > "$continuation_source"
LG_CACHE_DIR="$multi_dir/state-cache" LG_COMPILE_CACHE_DEBUG=1 \
  "$cli" --compile-files-from "$base_state" "$main_source" \
    "$continuation_source" -o "$partial_output" 2> "$partial_cache_stderr"
grep -q "compile cache hit: $main_source" "$partial_cache_stderr"
if grep -q "compile cache hit: $continuation_source" "$partial_cache_stderr"; then
  echo "changed saved-state suffix reused stale compile cache" >&2
  exit 1
fi
grep -q 'let continued_value = Math.magnitude_plus_two' "$partial_output"

cache_cli_root="$multi_dir/cache-cli-root"
cache_cli="$cache_cli_root/bin/lg"
cache_compiler_dir="$cache_cli_root/src"
source_compiler_dir="$(dirname "$cli")/../src"
cache_artifact_stderr="$multi_dir/cache-artifact.stderr"
mkdir -p "$cache_cli_root/bin" "$cache_compiler_dir"
cp "$cli" "$cache_cli"
cp "$source_compiler_dir/lg.cma" "$cache_compiler_dir/lg.cma"
cp "$source_compiler_dir/lg.cmxa" "$cache_compiler_dir/lg.cmxa"
cp "$source_compiler_dir/lg.a" "$cache_compiler_dir/lg.a"
chmod u+w "$cache_compiler_dir/lg.cmxa" "$cache_compiler_dir/lg.a"

LG_CACHE_DIR="$multi_dir/artifact-cache" \
  "$cache_cli" --compile-files "$math_source" "$main_source" -o "$multi_output"
stable_artifact_stderr="$multi_dir/stable-artifact.stderr"
printf 'changed-native-archive' >> "$cache_compiler_dir/lg.a"
LG_CACHE_DIR="$multi_dir/artifact-cache" LG_COMPILE_CACHE_DEBUG=1 \
  "$cache_cli" --compile-files "$math_source" "$main_source" -o "$multi_output" \
  2> "$stable_artifact_stderr"
grep -q "compile cache hit: $main_source" "$stable_artifact_stderr"

printf 'changed-native-compiler' >> "$cache_compiler_dir/lg.cmxa"
LG_CACHE_DIR="$multi_dir/artifact-cache" LG_COMPILE_CACHE_DEBUG=1 \
  "$cache_cli" --compile-files "$math_source" "$main_source" -o "$multi_output" \
  2> "$cache_artifact_stderr"
if grep -q "compile cache hit:" "$cache_artifact_stderr"; then
  echo "native compiler artifact change reused stale compile cache" >&2
  exit 1
fi

"$cli" --run-files "$math_source" "$main_source" > "$multi_stdout"
[ "$(cat "$multi_stdout")" = "42" ]

bad_source="$multi_dir/bad.cljc"
bad_stderr="$multi_dir/bad.stderr"
watched_provider="$multi_dir/watched-provider.cljc"
watched_consumer="$multi_dir/watched-consumer.cljc"
printf '%s\n' \
  '(def bad (Stdlib.abs "bad"))' > "$bad_source"
printf '%s\n' '(module Watched (def value 42))' > "$watched_provider"
printf '%s\n' '(def watched Watched/value)' > "$watched_consumer"

if "$cli" --compile-files "$math_source" "$bad_source" -o "$multi_output" \
    2> "$bad_stderr"; then
  echo "expected invalid multi-file compilation to fail" >&2
  exit 1
fi

grep -q "File \"$bad_source\", line 1" "$bad_stderr"

lsp_output="$multi_dir/lsp.output"

send_lsp_message() {
  message="$1"
  length="$(LC_ALL=C printf '%s' "$message" | wc -c | tr -d ' ')"
  printf 'Content-Length: %s\r\n\r\n%s' "$length" "$message"
}

{
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"rootUri\":\"file://$multi_dir\",\"capabilities\":{\"workspace\":{\"didChangeWatchedFiles\":{\"dynamicRegistration\":true}}}}}"
  send_lsp_message '{"jsonrpc":"2.0","method":"initialized","params":{}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/editor.cljc","languageId":"lg","version":1,"text":"(def answer\n  (if true\n    (Stdlib.abs\n      \"bad\")\n    0))"}}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didChange","params":{"textDocument":{"uri":"file:///tmp/editor.cljc","version":2},"contentChanges":[{"text":"(def ok 1)\n(def good (Stdlib.abs -42))"}]}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/semantic-error.cljc","languageId":"lg","version":1,"text":"(def ok 1)\n(def bad\n  (+ 1 \"x\"))"}}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/warning.cljc","languageId":"lg","version":1,"text":"(type-variant status Active Inactive)\n(defn describe [^:status status]\n  (match status Active \"active\"))"}}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/service.cljc","languageId":"lg","version":1,"text":"(def answer 41)\n(defn add-one [x] (+ x 1))\n(def result (add-one answer))"}}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/format.cljc","languageId":"lg","version":1,"text":"(def   answer  41)"}}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"file:///tmp/incomplete.cljc","languageId":"lg","version":1,"text":"(def answer 41)\n(def broken (+ answer"}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":3,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":14}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":4,"method":"textDocument/definition","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":22}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":5,"method":"textDocument/completion","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":29}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":6,"method":"textDocument/formatting","params":{"textDocument":{"uri":"file:///tmp/format.cljc"},"options":{"tabSize":2,"insertSpaces":true}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":7,"method":"textDocument/references","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":22},"context":{"includeDeclaration":true}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":8,"method":"textDocument/documentHighlight","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":22}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":9,"method":"textDocument/prepareRename","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":22}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":10,"method":"textDocument/rename","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":22},"newName":"total"}}'
  send_lsp_message '{"jsonrpc":"2.0","id":11,"method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"file:///tmp/service.cljc"}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":12,"method":"workspace/symbol","params":{"query":"add"}}'
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didOpen\",\"params\":{\"textDocument\":{\"uri\":\"file://$main_source\",\"languageId\":\"lg\",\"version\":1,\"text\":\"(println (Math/magnitude-plus-two 40))\\n\"}}}"
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"id\":13,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file://$main_source\"},\"position\":{\"line\":0,\"character\":14}}}"
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"id\":14,\"method\":\"textDocument/references\",\"params\":{\"textDocument\":{\"uri\":\"file://$main_source\"},\"position\":{\"line\":0,\"character\":14},\"context\":{\"includeDeclaration\":true}}}"
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"id\":15,\"method\":\"textDocument/rename\",\"params\":{\"textDocument\":{\"uri\":\"file://$main_source\"},\"position\":{\"line\":0,\"character\":14},\"newName\":\"distance-plus-two\"}}"
  send_lsp_message '{"jsonrpc":"2.0","id":16,"method":"workspace/symbol","params":{"query":"magnitude-plus-two"}}'
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"id\":17,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file://$watched_consumer\"},\"position\":{\"line\":0,\"character\":13}}}"
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"method\":\"workspace/didChangeWatchedFiles\",\"params\":{\"changes\":[{\"uri\":\"file://$watched_provider\",\"type\":3}]}}"
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"id\":18,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file://$watched_consumer\"},\"position\":{\"line\":0,\"character\":13}}}"
  send_lsp_message "{\"jsonrpc\":\"2.0\",\"method\":\"textDocument/didChange\",\"params\":{\"textDocument\":{\"uri\":\"file://$math_source\",\"version\":2},\"contentChanges\":[{\"text\":\"(module Math (def value 1))\\n\"}]}}"
  send_lsp_message '{"jsonrpc":"2.0","id":19,"method":"textDocument/semanticTokens/full","params":{"textDocument":{"uri":"file:///tmp/service.cljc"}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":20,"method":"textDocument/signatureHelp","params":{"textDocument":{"uri":"file:///tmp/service.cljc"},"position":{"line":2,"character":27}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":21,"method":"textDocument/hover","params":{"textDocument":{"uri":"file:///tmp/incomplete.cljc"},"position":{"line":0,"character":6}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":22,"method":"textDocument/codeAction","params":{"textDocument":{"uri":"file:///tmp/incomplete.cljc"},"range":{"start":{"line":1,"character":21},"end":{"line":1,"character":21}},"context":{"diagnostics":[]}}}'
  send_lsp_message '{"jsonrpc":"2.0","method":"textDocument/didClose","params":{"textDocument":{"uri":"file:///tmp/editor.cljc"}}}'
  send_lsp_message '{"jsonrpc":"2.0","id":2,"method":"shutdown","params":null}'
  send_lsp_message '{"jsonrpc":"2.0","method":"exit","params":null}'
} | "$cli" --lsp > "$lsp_output"

grep -q '"name":"lg"' "$lsp_output"
grep -q '"hoverProvider":true' "$lsp_output"
grep -q '"definitionProvider":true' "$lsp_output"
grep -q '"completionProvider"' "$lsp_output"
grep -q '"documentFormattingProvider":true' "$lsp_output"
grep -q '"codeActionProvider":true' "$lsp_output"
grep -q '"referencesProvider":true' "$lsp_output"
grep -q '"documentHighlightProvider":true' "$lsp_output"
grep -q '"renameProvider"' "$lsp_output"
grep -q '"documentSymbolProvider":true' "$lsp_output"
grep -q '"workspaceSymbolProvider":true' "$lsp_output"
grep -q '"semanticTokensProvider"' "$lsp_output"
grep -q '"signatureHelpProvider"' "$lsp_output"
grep -q '"method":"client/registerCapability"' "$lsp_output"
grep -Fq '"globPattern":"**/*.cljc"' "$lsp_output"
grep -q '"method":"textDocument/publishDiagnostics"' "$lsp_output"
grep -q '"severity":1' "$lsp_output"
grep -q '"severity":2' "$lsp_output"
grep -q 'not exhaustive' "$lsp_output"
grep -Fq '"uri":"file:///tmp/semantic-error.cljc","diagnostics":[{"range"' "$lsp_output"
grep -Fq '"uri":"file:///tmp/warning.cljc","diagnostics":[{"range"' "$lsp_output"
grep -q '"line":3' "$lsp_output"
grep -q '"character":6' "$lsp_output"
grep -q '"diagnostics":\[\]' "$lsp_output"
grep -q '"id":3,"result"' "$lsp_output"
grep -q '"id":4,"result"' "$lsp_output"
grep -q '"id":5,"result"' "$lsp_output"
grep -q '"id":6,"result"' "$lsp_output"
grep -q '"id":7,"result"' "$lsp_output"
grep -q '"id":8,"result"' "$lsp_output"
grep -q '"id":9,"result"' "$lsp_output"
grep -q '"id":10,"result"' "$lsp_output"
grep -q '"id":11,"result"' "$lsp_output"
grep -q '"id":12,"result"' "$lsp_output"
grep -q "\"id\":13,\"result\":{\"uri\":\"file://$math_source\"" "$lsp_output"
grep -q "\"id\":14,\"result\":.*\"uri\":\"file://$math_source\"" "$lsp_output"
grep -q "\"id\":14,\"result\":.*\"uri\":\"file://$main_source\"" "$lsp_output"
grep -q '"id":15,"result":{"changes"' "$lsp_output"
grep -q '"id":16,"result":\[{"name":"magnitude-plus-two"' "$lsp_output"
grep -q "\"id\":17,\"result\":{\"uri\":\"file://$watched_provider\"" "$lsp_output"
grep -q '"id":18,"result":null' "$lsp_output"
grep -q '"id":19,"result":{"data":\[[0-9]' "$lsp_output"
grep -q '"id":20,"result":{"signatures":\[{"label":"add-one : int -> int"' "$lsp_output"
grep -q '"id":21,"result":{"contents"' "$lsp_output"
grep -Fq '"id":22,"result":[{"title":"Insert missing )","kind":"quickfix","isPreferred":true' "$lsp_output"
grep -Fq '"newText":")"' "$lsp_output"
grep -Fq "\"uri\":\"file://$watched_consumer\",\"diagnostics\":[{\"range\"" "$lsp_output"
grep -q 'int -> int' "$lsp_output"
grep -q '"label":"add-one"' "$lsp_output"
grep -Fq '"newText":"(def answer 41)\n"' "$lsp_output"
grep -q '"newText":"total"' "$lsp_output"
grep -q '"name":"add-one"' "$lsp_output"
grep -q "\"uri\":\"file://$math_source\"" "$lsp_output"
grep -q "\"uri\":\"file://$main_source\"" "$lsp_output"
grep -q '"newText":"distance-plus-two"' "$lsp_output"
grep -q '"name":"magnitude-plus-two"' "$lsp_output"
grep -Fq "\"uri\":\"file://$main_source\",\"diagnostics\":[]" "$lsp_output"
grep -Fq "\"uri\":\"file://$main_source\",\"diagnostics\":[{\"range\"" "$lsp_output"
