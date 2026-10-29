# HeatFlowBLAS.cmake
#
# Detect and link BLAS/LAPACK for HeatFlow.
#
# Inputs:
#   HEATFLOW_BLAS_BACKEND   auto|accelerate|openblas|mkl|reference|none
#   HEATFLOW_BLAS_LIBRARIES explicit linker flags/libraries (optional)
#   HF_OPENBLAS_PREFIX      local OpenBLAS prefix (optional, from `make deps-blas`)
#
# Outputs:
#   HEATFLOW_BLAS_FOUND
#   HEATFLOW_BLAS_LIBRARIES
#   HEATFLOW_BLAS_NOTE

set(_HF_BLAS_HINTS
  "$ENV{CONDA_PREFIX}/lib"
  "$ENV{CONDA_PREFIX}/lib64"
  "$ENV{MKLROOT}/lib/intel64"
  "$ENV{MKLROOT}/lib"
  "$ENV{OPENBLAS_ROOT}/lib"
  "$ENV{OPENBLAS_HOME}/lib"
  "$ENV{EBROOTOPENBLAS}/lib"
  "$ENV{AOCL_ROOT}/lib"
  "$ENV{ARMPL_DIR}/lib"
  "/opt/intel/oneapi/mkl/latest/lib/intel64"
  "/opt/intel/oneapi/mkl/latest/lib"
  "/opt/homebrew/lib"
  "/opt/homebrew/opt/openblas/lib"
  "/usr/local/lib"
  "/usr/local/lib64"
  "/usr/local/opt/openblas/lib"
  "/opt/local/lib"
  "/usr/lib/x86_64-linux-gnu"
  "/usr/lib/aarch64-linux-gnu"
  "/usr/lib64"
  "/usr/lib"
)

if (HF_OPENBLAS_PREFIX)
  list(PREPEND _HF_BLAS_HINTS "${HF_OPENBLAS_PREFIX}/lib")
endif()

set(_HF_BLAS_BACKEND "${HEATFLOW_BLAS_BACKEND}")

macro(_hf_use_accelerate)
  set(_hf_found TRUE)
  set(_hf_libs "-framework;Accelerate")
  set(_hf_note "Apple Accelerate")
endmacro()

macro(_hf_try_openblas)
  set(_hf_found FALSE)
  if (HF_OPENBLAS_PREFIX)
    unset(HF_OPENBLAS_LIBRARY CACHE)
  endif()
  find_package(PkgConfig QUIET)
  if (PkgConfig_FOUND)
    pkg_check_modules(HF_OPENBLAS_PC QUIET openblas)
  endif()
  find_library(HF_OPENBLAS_LIBRARY NAMES openblas
    HINTS ${HF_OPENBLAS_PC_LIBRARY_DIRS} ${_HF_BLAS_HINTS})
  if (HF_OPENBLAS_LIBRARY)
    set(_hf_found TRUE)
    set(_hf_libs "${HF_OPENBLAS_LIBRARY}")
    if (HF_OPENBLAS_PC_FOUND)
      set(_hf_note "OpenBLAS via pkg-config")
    else()
      set(_hf_note "OpenBLAS: ${HF_OPENBLAS_LIBRARY}")
    endif()
  endif()
endmacro()

macro(_hf_try_mkl)
  set(_hf_found FALSE)
  find_package(PkgConfig QUIET)
  if (PkgConfig_FOUND)
    pkg_check_modules(HF_MKL_PC QUIET
      mkl-sdl mkl-dynamic-lp64-gomp mkl-dynamic-lp64-iomp mkl-dynamic-lp64-seq)
  endif()
  find_library(HF_MKL_LIBRARY NAMES mkl_rt
    HINTS ${HF_MKL_PC_LIBRARY_DIRS} ${_HF_BLAS_HINTS})
  if (HF_MKL_LIBRARY)
    set(_hf_found TRUE)
    set(_hf_libs "${HF_MKL_LIBRARY}")
    if (HF_MKL_PC_FOUND)
      set(_hf_note "MKL via pkg-config")
    else()
      set(_hf_note "MKL: ${HF_MKL_LIBRARY}")
    endif()
  endif()
endmacro()

macro(_hf_try_reference)
  set(_hf_found FALSE)
  find_library(HF_BLAS_LIBRARY NAMES blas HINTS ${_HF_BLAS_HINTS})
  find_library(HF_LAPACK_LIBRARY NAMES lapack HINTS ${_HF_BLAS_HINTS})
  if (HF_BLAS_LIBRARY AND HF_LAPACK_LIBRARY)
    set(_hf_found TRUE)
    set(_hf_libs "${HF_LAPACK_LIBRARY}" "${HF_BLAS_LIBRARY}")
    set(_hf_note "reference BLAS/LAPACK")
  else()
    find_package(BLAS QUIET)
    find_package(LAPACK QUIET)
    if (BLAS_FOUND AND LAPACK_FOUND)
      set(_hf_found TRUE)
      set(_hf_libs ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
      set(_hf_note "reference BLAS/LAPACK via CMake")
    endif()
  endif()
endmacro()

if (HEATFLOW_BLAS_LIBRARIES)
  set(HEATFLOW_BLAS_FOUND TRUE)
  set(HEATFLOW_BLAS_LIBRARIES "${HEATFLOW_BLAS_LIBRARIES}")
  set(HEATFLOW_BLAS_NOTE "user-specified BLAS/LAPACK")
  return()
endif()

if (_HF_BLAS_BACKEND STREQUAL "none")
  set(HEATFLOW_BLAS_FOUND FALSE)
  set(HEATFLOW_BLAS_LIBRARIES "")
  set(HEATFLOW_BLAS_NOTE "disabled")
  return()
endif()

if (NOT _HF_BLAS_BACKEND MATCHES "^(auto|accelerate|openblas|mkl|reference)$")
  message(FATAL_ERROR
    "Unknown HEATFLOW_BLAS_BACKEND='${_HF_BLAS_BACKEND}'. "
    "Use auto, accelerate, openblas, mkl, reference, download or none.")
endif()

if (_HF_BLAS_BACKEND STREQUAL "accelerate")
  if (NOT APPLE)
    message(FATAL_ERROR "HEATFLOW_BLAS_BACKEND=accelerate is only available on macOS")
  endif()
  _hf_use_accelerate()
elseif (_HF_BLAS_BACKEND STREQUAL "openblas")
  _hf_try_openblas()
  if (NOT _hf_found)
    message(FATAL_ERROR
      "OpenBLAS requested but not found. Install it, set HEATFLOW_BLAS_LIBRARIES, "
      "or configure with -DHEATFLOW_BLAS_DOWNLOAD=ON.")
  endif()
elseif (_HF_BLAS_BACKEND STREQUAL "mkl")
  _hf_try_mkl()
  if (NOT _hf_found)
    message(FATAL_ERROR
      "MKL requested but not found. Install it, set HEATFLOW_BLAS_LIBRARIES, "
      "or use -DHEATFLOW_BLAS_BACKEND=auto.")
  endif()
elseif (_HF_BLAS_BACKEND STREQUAL "reference")
  _hf_try_reference()
  if (NOT _hf_found)
    message(FATAL_ERROR
      "Reference BLAS/LAPACK requested but not found. Install them or set "
      "HEATFLOW_BLAS_LIBRARIES.")
  endif()
else() # auto
  if (APPLE AND NOT HF_OPENBLAS_PREFIX)
    _hf_use_accelerate()
  else()
    _hf_try_openblas()
    if (NOT _hf_found)
      _hf_try_mkl()
    endif()
    if (NOT _hf_found)
      _hf_try_reference()
    endif()
    if (NOT _hf_found)
      set(HEATFLOW_BLAS_FOUND FALSE)
      set(HEATFLOW_BLAS_LIBRARIES "")
      set(HEATFLOW_BLAS_NOTE "not found (optional)")
      return()
    endif()
  endif()
endif()

set(HEATFLOW_BLAS_FOUND ${_hf_found})
set(HEATFLOW_BLAS_LIBRARIES ${_hf_libs})
set(HEATFLOW_BLAS_NOTE "${_hf_note}")
