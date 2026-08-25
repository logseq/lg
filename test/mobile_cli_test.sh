#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
mobile="$repo_root/scripts/lg-mobile"
android_setup="$repo_root/scripts/mobile/bootstrap_android_ocaml.sh"

if grep -Fq 'ANDROID_NDK_HOME does not contain a complete NDK toolchain' "$android_setup"; then
  echo "Android setup does not fall back from an incomplete NDK override" >&2
  exit 1
fi

ios_simulator=$($mobile build ios simulator --dry-run)
[[ $ios_simulator == 'ios-simulator arm64-apple-ios17.0-simulator' ]]

ios_device=$($mobile build ios device --dry-run)
[[ $ios_device == 'ios-device arm64-apple-ios17.0' ]]

android=$($mobile build android --dry-run)
[[ $android == 'android arm64-v8a aarch64-linux-android21' ]]

if $mobile build --dry-run >/dev/null 2>&1; then
  echo "mobile build accepted a missing platform" >&2
  exit 1
fi

if $mobile build ios --dry-run >/dev/null 2>&1; then
  echo "iOS build accepted a missing environment" >&2
  exit 1
fi

help=$($mobile --help)
grep -Fq 'lg mobile build' <<<"$help"
if grep -Eq -- '--profile|--target' <<<"$help"; then
  echo "mobile help still exposes platform selection as profiles" >&2
  exit 1
fi

echo "ok - LG mobile CLI requires an explicit platform and iOS environment"
