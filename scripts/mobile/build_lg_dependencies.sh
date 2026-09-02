#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 TARGET_PREFIX BUILD_DIRECTORY" >&2
  exit 2
fi

target_prefix=$(cd "$1" && pwd)
mkdir -p "$2"
build_root=$(cd "$2" && pwd)
ocamlopt="$target_prefix/bin/ocamlopt.opt"
ocamldep="$target_prefix/bin/ocamldep.opt"

die() {
  echo "error: $*" >&2
  exit 1
}

command -v ocamlfind >/dev/null 2>&1 || die "ocamlfind was not found"
[[ -x $ocamlopt && -x $ocamldep ]] || die "target OCaml compiler is incomplete"
[[ $($ocamlopt -version) == $(ocamlopt -version) ]] \
  || die "host and target OCaml versions must match"

source_root="$build_root/sources"
object_root="$build_root/objects"
rm -rf "$source_root" "$object_root"
mkdir -p "$source_root" "$object_root"

copy_sources() {
  local package=$1
  local destination=$2
  local package_dir
  package_dir=$(ocamlfind query "$package")
  mkdir -p "$destination"
  for source in "$package_dir"/*.ml "$package_dir"/*.mli; do
    [[ -f $source ]] || continue
    [[ $(basename "$source") == *__.* ]] && continue
    cp "$source" "$destination/"
  done
}

compile_sources() {
  local source_dir=$1
  local object_dir=$2
  shift 2
  mkdir -p "$object_dir"
  local include_args=(-I "$object_dir")
  local dependency
  for dependency in "$@"; do
    include_args+=(-I "$dependency")
  done

  local ordered_sources
  ordered_sources=$(cd "$source_dir" && "$ocamldep" "${include_args[@]}" -sort ./*.mli ./*.ml)
  local source basename output
  for source in $ordered_sources; do
    basename=${source#./}
    case "$basename" in
      *.mli) output="$object_dir/${basename%.mli}.cmi" ;;
      *.ml) output="$object_dir/${basename%.ml}.cmx" ;;
      *) continue ;;
    esac
    "$ocamlopt" "${include_args[@]}" -w -9-49 -c \
      "$source_dir/$basename" -o "$output"
  done

  "$ocamldep" -sort "$source_dir"/*.ml \
    | tr ' ' '\n' \
    | sed '/^$/d; s#^.*/##; s#\.ml$#.cmx#' \
    | while IFS= read -r object; do
        echo "$object_dir/$object"
      done >"$object_dir/link-objects.txt"
}

yojson_source="$source_root/yojson"
yojson_objects="$object_root/yojson"
copy_sources yojson "$yojson_source"
echo 'include Common' >"$yojson_source/yojson__Common.ml"
compile_sources "$yojson_source" "$yojson_objects"

edn_core_source="$source_root/melange-edn-core"
edn_core_objects="$object_root/melange-edn-core"
copy_sources melange-edn-core "$edn_core_source"
compile_sources "$edn_core_source" "$edn_core_objects"

edn_native_source="$source_root/melange-edn-native"
edn_native_objects="$object_root/melange-edn-native"
copy_sources melange-edn-native "$edn_native_source"
compile_sources "$edn_native_source" "$edn_native_objects" \
  "$edn_core_objects" "$yojson_objects"

re_source="$source_root/re"
re_objects="$object_root/re"
copy_sources re "$re_source"
cp "$(ocamlfind query re)/re__.ml" "$re_source/re__.ml"
mkdir -p "$re_objects"
"$ocamlopt" -I "$re_objects" -no-alias-deps -opaque -w -49 -c \
  "$re_source/re__.ml" -o "$re_objects/re__.cmx"
re_internal_sources=()
for source in "$re_source"/*.ml "$re_source"/*.mli; do
  case "$(basename "$source")" in
    re.ml|re.mli|re__.ml) ;;
    *) re_internal_sources+=("$source") ;;
  esac
done
re_ordered_sources=$("$ocamldep" -I "$re_objects" -open Re__ -sort \
  "${re_internal_sources[@]}")
printf '%s\n' "$re_objects/re__.cmx" >"$re_objects/link-objects.txt"
for source in $re_ordered_sources; do
  source_file=$(basename "$source")
  source_name=${source_file%.*}
  module_name="${source_name^}"
  case "$source" in
    *.mli) output="$re_objects/re__${module_name}.cmi" ;;
    *.ml)
      output="$re_objects/re__${module_name}.cmx"
      printf '%s\n' "$output" >>"$re_objects/link-objects.txt"
      ;;
    *) continue ;;
  esac
  "$ocamlopt" -I "$re_objects" -open Re__ -c "$source" -o "$output"
done
if [[ -f "$re_source/re.mli" ]]; then
  "$ocamlopt" -I "$re_objects" -open Re__ -c "$re_source/re.mli" \
    -o "$re_objects/re.cmi"
fi
"$ocamlopt" -I "$re_objects" -open Re__ -c "$re_source/re.ml" \
  -o "$re_objects/re.cmx"
printf '%s\n' "$re_objects/re.cmx" >>"$re_objects/link-objects.txt"

rrbvec_source="$source_root/rrbvec"
rrbvec_objects="$object_root/rrbvec"
copy_sources lg.rrbvec "$rrbvec_source"
compile_sources "$rrbvec_source" "$rrbvec_objects"

backend_source="$source_root/lg-edn-backend"
backend_objects="$object_root/lg-edn-backend"
mkdir -p "$backend_source"
cp "$(ocamlfind query lg.edn-backend)/edn_backend.mli" \
  "$backend_source/lg_edn_backend.mli"
cp "$(ocamlfind query lg.edn-backend.native)/edn_backend.ml" \
  "$backend_source/lg_edn_backend.ml"
compile_sources "$backend_source" "$backend_objects" \
  "$edn_core_objects" "$edn_native_objects" "$yojson_objects" "$re_objects"

runtime_source="$source_root/lg-runtime"
runtime_objects="$object_root/lg-runtime"
copy_sources lg.runtime "$runtime_source"
rm -f "$runtime_source/lg_runtime.ml"
for source in "$runtime_source"/*_melange.ml "$runtime_source"/*_melange.mli; do
  [[ -f $source ]] || continue
  [[ $(basename "$source") == runtime_int_melange.ml ]] && continue
  rm -f "$source"
done
compile_sources "$runtime_source" "$runtime_objects" \
  "$backend_objects" "$rrbvec_objects"

runtime_wrapper="$runtime_source/lg_runtime.ml"
awk 'BEGIN { RS=""; ORS="\n\n" } $0 !~ /_melange/ { gsub(/Lg_runtime__/, ""); print }' \
  "$(ocamlfind query lg.runtime)/lg_runtime.ml" >"$runtime_wrapper"
"$ocamlopt" \
  -I "$runtime_objects" \
  -I "$backend_objects" \
  -I "$rrbvec_objects" \
  -no-alias-deps \
  -opaque \
  -c "$runtime_wrapper" \
  -o "$runtime_objects/lg_runtime.cmx"
echo "$runtime_objects/lg_runtime.cmx" >>"$runtime_objects/link-objects.txt"

: >"$build_root/link-objects.txt"
for list in \
  "$yojson_objects/link-objects.txt" \
  "$edn_core_objects/link-objects.txt" \
  "$edn_native_objects/link-objects.txt" \
  "$re_objects/link-objects.txt" \
  "$rrbvec_objects/link-objects.txt" \
  "$backend_objects/link-objects.txt" \
  "$runtime_objects/link-objects.txt"; do
  cat "$list" >>"$build_root/link-objects.txt"
done

for directory in \
  "$yojson_objects" \
  "$edn_core_objects" \
  "$edn_native_objects" \
  "$re_objects" \
  "$rrbvec_objects" \
  "$backend_objects" \
  "$runtime_objects"; do
  echo "$directory"
done >"$build_root/include-directories.txt"

echo "$build_root"
