#!/usr/bin/env bash
# Build HeatFlow with fpm and PETSc support.
#
# PETSc paths are taken from the standard PETSc variables:
#   export PETSC_DIR=/path/to/petsc
#   export PETSC_ARCH=arch-your-build   # optional for an installed PETSc
#
# BLAS/LAPACK handling is shared with the Makefile:
#   BLAS=0                  disable BLAS/LAPACK
#   BLAS_LIBS='...'         explicit linker flags
#   BLAS_BACKEND=auto|...   backend selector
#   BLAS_BACKEND=download   opt-in: build OpenBLAS via `make deps-blas`
#
# Extra arguments are passed to `fpm build`.

set -euo pipefail

if [ -z "${PETSC_DIR:-}" ]; then
  echo "ERROR: PETSC_DIR is not set." >&2
  exit 1
fi
if [ ! -d "${PETSC_DIR}" ]; then
  echo "ERROR: PETSC_DIR '${PETSC_DIR}' does not exist." >&2
  exit 1
fi
if ! command -v fpm >/dev/null 2>&1; then
  echo "ERROR: fpm was not found in PATH." >&2
  exit 1
fi

# Resolve PETSC_DIR relative to the invocation directory before changing it.
petsc_dir="$(cd "${PETSC_DIR}" && pwd)"
petsc_arch="${PETSC_ARCH:-}"

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$script_dir"

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

fflags="-DHAVE_PETSC -I${petsc_dir}/include"
petsc_libdir="${petsc_dir}/lib"
if [ -n "$petsc_arch" ]; then
  fflags="$fflags -I${petsc_dir}/${petsc_arch}/include"
  petsc_libdir="${petsc_dir}/${petsc_arch}/lib"
fi
if [ ! -d "$petsc_libdir" ]; then
  echo "ERROR: PETSc library directory '${petsc_libdir}' does not exist." >&2
  exit 1
fi
if [ -n "${FPM_FFLAGS:-}" ]; then
  fflags="$fflags ${FPM_FFLAGS}"
fi

ldflags="-L${petsc_libdir} -Wl,-rpath,${petsc_libdir} -lpetsc"
if [ -n "$blas_flags" ]; then
  ldflags="$ldflags $blas_flags"
fi
if [ -n "${FPM_LDFLAGS:-}" ]; then
  ldflags="$ldflags ${FPM_LDFLAGS}"
fi

if [ -n "$blas_note" ]; then
  echo "[fpm] BLAS/LAPACK: ${blas_note}"
fi

profile="${FPM_PROFILE:-release}"
echo "[fpm] fpm build --profile ${profile} (PETSc: ${petsc_dir}${petsc_arch:+/${petsc_arch}})"
exec fpm build --profile "$profile" \
  --flag "$fflags" \
  --link-flag "$ldflags" \
  "$@"
