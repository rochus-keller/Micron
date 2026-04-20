#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "usage: $0 <module-name-or-file.mic>" >&2
  exit 1
fi

input="$1"
module="${input%.mic}"

./micc "${module}.mic" -a x86 --cdecl -I../oakwood -L. -n micron_i386 
