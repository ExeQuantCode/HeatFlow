#!/usr/bin/env bash

: "${PETSC_DIR:?Need to set PETSC_DIR}"
: "${PETSC_ARCH:?Need to set PETSC_ARCH}"

fpm build \
  --flag "-I${PETSC_DIR}/include -I${PETSC_DIR}/${PETSC_ARCH}/include" \
  --link-flag "-L${PETSC_DIR}/${PETSC_ARCH}/lib -Wl,-rpath,${PETSC_DIR}/${PETSC_ARCH}/lib"