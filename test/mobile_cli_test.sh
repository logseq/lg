#!/usr/bin/env bash

set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
mobile="$repo_root/scripts/lg-mobile"

development=$($mobile build --profile development --dry-run)
grep -Fq 'ios-simulator arm64-apple-ios17.0-simulator' <<<"$development"
grep -Fq 'android arm64-v8a aarch64-linux-android21' <<<"$development"
if grep -Fq 'ios-device' <<<"$development"; then
  echo "development unexpectedly includes an iOS device target" >&2
  exit 1
fi

release=$($mobile build --profile release --dry-run)
grep -Fq 'ios-device arm64-apple-ios17.0' <<<"$release"
grep -Fq 'android arm64-v8a aarch64-linux-android21' <<<"$release"
if grep -Fq 'ios-simulator' <<<"$release"; then
  echo "release unexpectedly includes an iOS simulator target" >&2
  exit 1
fi

all_targets=$($mobile build --profile all --dry-run)
grep -Fq 'ios-simulator arm64-apple-ios17.0-simulator' <<<"$all_targets"
grep -Fq 'ios-device arm64-apple-ios17.0' <<<"$all_targets"
grep -Fq 'android arm64-v8a aarch64-linux-android21' <<<"$all_targets"

ios_only=$($mobile build --target ios-simulator --dry-run)
[[ $ios_only == 'ios-simulator arm64-apple-ios17.0-simulator' ]]

android_only=$($mobile build --target android --dry-run)
[[ $android_only == 'android arm64-v8a aarch64-linux-android21' ]]

help=$($mobile --help)
grep -Fq 'lg mobile build' <<<"$help"
grep -Fq 'development' <<<"$help"
grep -Fq 'release' <<<"$help"
grep -Fq 'all' <<<"$help"

echo "ok - LG mobile CLI exposes one profile-based build command"
