#!/usr/bin/env bash
# Runs every solution file that uses lib/check.scm (or the files given as
# arguments) with MIT Scheme and reports the ones that fail.
# Paths are relative to this directory.
set -u
cd "$(dirname "$0")"

if [ "$#" -gt 0 ]; then
  files=("$@")
else
  mapfile -t files < <(grep -rl --include='*.scm' --exclude-dir=lib '"lib/check.scm"' . |
                       sed 's|^\./||' | sort -V)
fi

failed=()
for file in "${files[@]}"; do
  if output=$(timeout 300 mit-scheme --quiet --load "$file" --eval '(exit 0)' </dev/null 2>&1); then
    echo "ok   $file"
  else
    echo "FAIL $file"
    echo "$output" | sed 's/^/     /' | tail -20
    failed+=("$file")
  fi
done

echo
echo "${#files[@]} files, ${#failed[@]} failed"
[ "${#failed[@]}" -eq 0 ]
