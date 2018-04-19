#!/usr/bin/env bash
set -e
set -o pipefail

for CONF in "$@"; do
  output_file="${CONF%.*}.json"
  # We write to a temporary and then move because in case of a JSON failure,
  # we do not want to create the output file, which prevents ninja from
  # rerunning the script.
  cat "$CONF" | auv-json-decomment > "${output_file}.tmp"
  mv "${output_file}.tmp" "${output_file}"
done
