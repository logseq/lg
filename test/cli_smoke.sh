#!/bin/sh
set -eu

cli="$1"
example="$2"

output="$($cli --run "$example")"

[ "$output" = "ADA:true:1" ]
