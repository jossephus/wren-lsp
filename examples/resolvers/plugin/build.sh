#!/usr/bin/env bash
set -euo pipefail

ext="so"
case "$(uname -s)" in
  Darwin) ext="dylib" ;;
  MINGW*|MSYS*|CYGWIN*) ext="dll" ;;
esac

cc -shared -fPIC -O2 -o "math_resolver.${ext}" math_resolver.c
