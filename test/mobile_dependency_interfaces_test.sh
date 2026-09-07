#!/usr/bin/env bash
set -euo pipefail
repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
host_prefix=$(opam var prefix --safe)
cat >"$work/external_consumer.ml" <<'ML'
let edn_identity (value : Lg_edn_backend.t) = value
let reference_identity (value : int Lg_runtime.Runtime_reference.t) = value
ML
ocamlfind ocamlopt -package lg.edn-backend.native,lg.runtime -c   "$work/external_consumer.ml" -o "$work/external_consumer.cmx"
bash "$repo_root/scripts/mobile/build_lg_dependencies.sh" "$host_prefix" "$work/mobile"
include_args=(-I "$work")
while IFS= read -r directory; do include_args+=(-I "$directory"); done   <"$work/mobile/include-directories.txt"
cat >"$work/mobile_consumer.ml" <<'ML'
let edn_value = External_consumer.edn_identity (Lg_edn_backend.Small_int 7)
let reference_value = External_consumer.reference_identity
  (Lg_runtime.Runtime_reference.of_value 7)
let () =
  assert (edn_value = Lg_edn_backend.Small_int 7);
  assert (Lg_runtime.Runtime_reference.deref reference_value = 7);
  assert (Lg_runtime.Runtime_time.now () > 0.)
ML
ocamlopt "${include_args[@]}" -c "$work/mobile_consumer.ml"   -o "$work/mobile_consumer.cmx"
objects=()
while IFS= read -r object; do objects+=("$object"); done <"$work/mobile/link-objects.txt"
ocamlfind ocamlopt -package unix,str -linkpkg "${include_args[@]}" \
  "${objects[@]}" "$work/external_consumer.cmx" "$work/mobile_consumer.cmx" \
  -o "$work/mobile_consumer.exe"
"$work/mobile_consumer.exe"
echo "Mobile dependencies preserve precompiled EDN and runtime type identities"
