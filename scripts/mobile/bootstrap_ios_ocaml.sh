#!/usr/bin/env bash

set -euo pipefail

die() {
  echo "error: $*" >&2
  exit 1
}

[[ $# -eq 1 ]] || die "usage: $0 simulator|device"
platform=$1
ocaml_version=${LG_MOBILE_OCAML_VERSION:-5.5.0}
deployment_target=${LG_IOS_DEPLOYMENT_TARGET:-17.0}
jobs=${LG_MOBILE_BUILD_JOBS:-8}
opam_root=$(opam var root --safe)
shared_root=${LG_OCAML_TOOLCHAIN_ROOT:-$opam_root/lg-ocaml-toolchains}
version_root="$shared_root/ocaml-$ocaml_version"

case "$platform" in
  simulator)
    sdk=iphonesimulator
    triple="arm64-apple-ios${deployment_target}-simulator"
    configure_target=aarch64-apple-darwin.simulator
    ;;
  device)
    sdk=iphoneos
    triple="arm64-apple-ios${deployment_target}"
    configure_target=aarch64-apple-darwin
    ;;
  *) die "unsupported iOS platform: $platform" ;;
esac

host_prefix="$version_root/host"
target_prefix="$version_root/targets/$triple"
host_source="$version_root/sources/host"
target_source="$version_root/sources/$triple"

[[ $(uname -s) == Darwin ]] || die "the iOS toolchain requires macOS"
command -v xcrun >/dev/null 2>&1 || die "xcrun was not found"
command -v opam >/dev/null 2>&1 || die "opam was not found"

sdk_path=$(xcrun --sdk "$sdk" --show-sdk-path)
clang=$(xcrun --sdk "$sdk" --find clang)
ar=$(xcrun --sdk "$sdk" --find ar)
ld=$(xcrun --sdk "$sdk" --find ld)
ranlib=$(xcrun --sdk "$sdk" --find ranlib)
strip=$(xcrun --sdk "$sdk" --find strip)
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
    PATH="$host_prefix/bin:$PATH" \
      ac_cv_func_getentropy=no \
      ac_cv_func_system=no \
      ./configure \
      --disable-dependency-generation \
      --disable-function-sections \
      --disable-shared \
      --disable-warn-error \
      --prefix="$target_prefix" \
      --target="$configure_target" \
      --without-zstd \
      TARGET_LIBDIR=/dummy/directory \
      CC="$clang -target $triple -isysroot $sdk_path" \
      AR="$ar" DIRECT_LD="$ld" LD="$ld" PARTIALLD="$ld -r" \
      RANLIB="$ranlib" STRIP="$strip"
    PATH="$host_prefix/bin:$PATH" make crossopt -j"$jobs"
    PATH="$host_prefix/bin:$PATH" make installcross
  )
fi

[[ $($target_prefix/bin/ocamlopt.opt -version) == "$ocaml_version" ]] \
  || die "target compiler version does not match $ocaml_version"
echo "$target_prefix"
