#!/usr/bin/env bash
# Build HeatFlow with fpm, linking an auto-detected BLAS/LAPACK backend.
#
# BLAS/LAPACK handling is shared with the Makefile:
#   BLAS=0                  disable BLAS/LAPACK
#   BLAS_LIBS='...'         explicit linker flags
#   BLAS_BACKEND=auto|...   backend selector
#   BLAS_BACKEND=download   opt-in: build OpenBLAS via `make deps-blas`
#
# Extra arguments are passed to `fpm build`, e.g. --tests --compiler mpifort.

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$script_dir"

if ! command -v fpm >/dev/null 2>&1; then
  echo "ERROR: fpm was not found in PATH." >&2
  exit 1
fi

if [ -z "${BLAS_LIBS:-}" ] && [ "${BLAS:-1}" != "0" ] \
   && [ "${BLAS_BACKEND:-auto}" = "download" ]; then
  if ! command -v make >/dev/null 2>&1; then
    echo "ERROR: BLAS_BACKEND=download requires make (used by 'make deps-blas')." >&2
    exit 1
  fi
  make deps-blas
fi

blas_flags=""
blas_note=""
if command -v make >/dev/null 2>&1; then
  blas_flags="$(make -s print-blas-flags)"
  blas_note="$(make -s print-blas-backend)"
else
  blas_flags="${BLAS_LIBS:-}"
  if [ -n "$blas_flags" ]; then
    blas_note="(user-specified BLAS/LAPACK)"
  else
    blas_note="(not linked; make not found for auto-detection)"
  fi
fi

if [ -n "$blas_note" ]; then
  echo "[fpm] BLAS/LAPACK: ${blas_note}"
fi

ldflags="${FPM_LDFLAGS:-}"
if [ -n "$blas_flags" ]; then
  ldflags="${ldflags:+$ldflags }$blas_flags"
fi

profile="${FPM_PROFILE:-release}"
fpm_args=(--profile "$profile")
if [ -n "${FPM_FFLAGS:-}" ]; then
  fpm_args+=(--flag "${FPM_FFLAGS}")
fi
if [ -n "$ldflags" ]; then
  fpm_args+=(--link-flag "$ldflags")
fi

echo "[fpm] fpm build --profile ${profile}"
exec fpm build "${fpm_args[@]}" "$@"
