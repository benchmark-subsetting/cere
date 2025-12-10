#!/bin/bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
BUILD="$ROOT/examples/kokkos/build"

mkdir -p "${BUILD}"
cd "${BUILD}"

export PYTHONPATH="${ROOT}/src:/usr/local/lib/python3.12/site-packages"
export PATH="${ROOT}/src:/usr/local/bin:${PATH}"
LIBDIR="${CERE_LIBDIR:-/usr/local/lib}"
export CERE_LIBDIR="${LIBDIR}"
export LD_LIBRARY_PATH="/usr/lib/x86_64-linux-gnu:/usr/lib/llvm-16/lib:${LIBDIR}:${LD_LIBRARY_PATH:-}"
if ! command -v python >/dev/null 2>&1; then
  PYTHON3_BIN=$(command -v python3 || true)
  if [ -n "${PYTHON3_BIN}" ]; then
    PYTHON_BIN_DIR="${BUILD}/tmp-python-bin"
    mkdir -p "${PYTHON_BIN_DIR}"
    ln -sf "${PYTHON3_BIN}" "${PYTHON_BIN_DIR}/python"
    export PATH="${PYTHON_BIN_DIR}:${PATH}"
  fi
fi

KOKKOS_SRC="${BUILD}/kokkos-src"
KOKKOS_BUILD="${BUILD}/kokkos-build"
KOKKOS_INSTALL="${BUILD}/kokkos-install"

BASE_SAFE_DIR="$(realpath "${KOKKOS_SRC}")"
mkdir -p "${KOKKOS_SRC}"
git config --global --add safe.directory "${BASE_SAFE_DIR}"
if [ ! -d "${KOKKOS_SRC}/.git" ]; then
  git clone --branch release-candidate-5.0.1 --depth 1 https://github.com/kokkos/kokkos.git "${KOKKOS_SRC}"
fi

rm -rf "${KOKKOS_BUILD}" || true
cmake -S "${KOKKOS_SRC}" -B "${KOKKOS_BUILD}" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_CXX_STANDARD=20 \
  -DKokkos_ENABLE_SERIAL=ON \
  -DKokkos_ENABLE_TESTS=OFF \
  -DKokkos_ENABLE_EXAMPLES=OFF \
  -DKokkos_ENABLE_OPENMP=OFF \
  -DKokkos_ENABLE_CUDA=OFF \
  -DCMAKE_INSTALL_PREFIX="${KOKKOS_INSTALL}"
cmake --build "${KOKKOS_BUILD}" --target install

if [ -d "${KOKKOS_INSTALL}/lib64" ]; then
  KOKKOS_LIB="${KOKKOS_INSTALL}/lib64"
else
  KOKKOS_LIB="${KOKKOS_INSTALL}/lib"
fi

cerebld="cerec++ -g -std=gnu++20 -I${KOKKOS_INSTALL}/include ${ROOT}/examples/kokkos/example.cpp ${ROOT}/examples/cpp23/rdtsc_support.cpp -L${KOKKOS_LIB} -lkokkoscore -lkokkossimd -lstdc++ -lm"

BUILD_CMD="${cerebld} -o a.out"

cere configure \
  --build-cmd="${BUILD_CMD}" \
  --run-cmd="./a.out" \
  --clean-cmd="rm -f a.out"

cere profile --force

if [[ -n "${CERE_REGION:-}" ]]; then
  CAPTURE_CMD="cere capture --region=${CERE_REGION}"
  [[ -n "${CERE_INVOCATION:-}" ]] && CAPTURE_CMD+=" --invocation=${CERE_INVOCATION}"
  CAPTURE_CMD+=" --force"
  ${CAPTURE_CMD}

  REPLAY_CMD="cere replay --region=${CERE_REGION}"
  [[ -n "${CERE_INVOCATION:-}" ]] && REPLAY_CMD+=" --invocation=${CERE_INVOCATION}"
  REPLAY_CMD+=" --force"
  ${REPLAY_CMD}
else
  echo "Skipping capture/replay: set CERE_REGION to the desired loop name (see .cere/report after profiling)"
fi
