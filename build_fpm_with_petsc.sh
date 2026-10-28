#!/usr/bin/env bash
# Build HeatFlow with fpm and PETSc support.
# PETSc include/library paths are taken from the standard PETSc variables:
#   export PETSC_DIR=/path/to/petsc
#   export PETSC_ARCH=arch-your-build

: "${PETSC_DIR:?Need to set PETSC_DIR}"
: "${PETSC_ARCH:?Need to set PETSC_ARCH}"

fpm build \
  --flag "-I${PETSC_DIR}/include -I${PETSC_DIR}/${PETSC_ARCH}/include" \
  --link-flag "-L${PETSC_DIR}/${PETSC_ARCH}/lib -Wl,-rpath,${PETSC_DIR}/${PETSC_ARCH}/lib"
