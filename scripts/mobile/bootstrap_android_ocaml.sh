#!/usr/bin/env bash

set -euo pipefail

die() {
  echo "error: $*" >&2
  exit 1
}

[[ $# -eq 1 ]] || die "usage: $0 arm64-v8a|x86_64"
android_abi=$1
ocaml_version=${LG_MOBILE_OCAML_VERSION:-5.5.0}
api_level=${LG_ANDROID_API_LEVEL:-21}
jobs=${LG_MOBILE_BUILD_JOBS:-8}
android_home=${ANDROID_HOME:-${ANDROID_SDK_ROOT:-$HOME/Library/Android/sdk}}
opam_root=$(opam var root --safe)
shared_root=${LG_OCAML_TOOLCHAIN_ROOT:-$opam_root/lg-ocaml-toolchains}
version_root="$shared_root/ocaml-$ocaml_version"

case "$android_abi" in
  arm64-v8a) target_arch=aarch64 ;;
  x86_64) target_arch=x86_64 ;;
  *) die "unsupported Android ABI: $android_abi" ;;
esac

target="${target_arch}-linux-android${api_level}"
host_prefix="$version_root/host"
target_prefix="$version_root/targets/$target"
host_source="$version_root/sources/host"
target_source="$version_root/sources/$target"

case "$(uname -s)" in
  Darwin) ndk_host=darwin-x86_64 ;;
  Linux) ndk_host=linux-x86_64 ;;
  *) die "unsupported build host: $(uname -s)" ;;
esac

ndk_root=
if [[ -n ${ANDROID_NDK_HOME:-} \
      && -x $ANDROID_NDK_HOME/toolchains/llvm/prebuilt/$ndk_host/bin/clang ]]; then
  ndk_root=$ANDROID_NDK_HOME
fi
if [[ -z $ndk_root ]]; then
  for candidate in "$android_home"/ndk/*; do
    [[ -x $candidate/toolchains/llvm/prebuilt/$ndk_host/bin/clang ]] \
      && ndk_root=$candidate
  done
fi
[[ -n $ndk_root ]] || die "Android NDK is not installed under $android_home/ndk"
ndk_bin="$ndk_root/toolchains/llvm/prebuilt/$ndk_host/bin"
mkdir -p "$version_root/sources" "$version_root/targets"

clone_release() {
  local destination=$1
  if [[ ! -d $destination/.git ]]; then
    git clone --depth 1 --branch "$ocaml_version" \
      https://github.com/ocaml/ocaml.git "$destination"
  fi
}

if [[ ! -x $host_prefix/bin/ocamlopt.opt ]]; then
  clone_release "$host_source"
  (
    cd "$host_source"
    ./configure --disable-ocamldoc --disable-ocamltest \
      --disable-stdlib-manpages --prefix="$host_prefix"
    make -j"$jobs"
    make install
  )
fi

[[ $($host_prefix/bin/ocamlopt.opt -version) == "$ocaml_version" ]] \
  || die "host compiler version does not match $ocaml_version"

if [[ ! -x $target_prefix/bin/ocamlopt.opt ]]; then
  clone_release "$target_source"
  (
    cd "$target_source"
    PATH="$host_prefix/bin:$PATH" ./configure \
      --disable-function-sections \
      --prefix="$target_prefix" \
      --target="$target" \
      TARGET_LIBDIR=/dummy/directory \
      CC="$ndk_bin/clang --target=$target" \
      AR="$ndk_bin/llvm-ar" \
      PARTIALLD="$ndk_bin/ld -r" \
      RANLIB="$ndk_bin/llvm-ranlib" \
      STRIP="$ndk_bin/llvm-strip"
    PATH="$host_prefix/bin:$PATH" make crossopt -j"$jobs"
    PATH="$host_prefix/bin:$PATH" make installcross
  )
fi

[[ $($target_prefix/bin/ocamlopt.opt -version) == "$ocaml_version" ]] \
  || die "target compiler version does not match $ocaml_version"
echo "$target_prefix"
