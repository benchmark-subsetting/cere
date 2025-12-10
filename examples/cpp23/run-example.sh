#!/bin/bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
BUILD="$ROOT/examples/cpp23/build"

mkdir -p "${BUILD}"
cd "${BUILD}"

export PYTHONPATH="${ROOT}/src:/usr/local/lib/python3.12/site-packages"
export PATH="${ROOT}/src:/usr/local/bin:${PATH}"
LIBDIR="${CERE_LIBDIR:-/usr/local/lib}"
export CERE_LIBDIR="${LIBDIR}"
export LD_LIBRARY_PATH="${LIBDIR}:${LD_LIBRARY_PATH:-}"

BUILD_CMD="cerec++ -std=gnu++2b -stdlib=libc++ ${ROOT}/examples/cpp23/example.cpp ${ROOT}/examples/cpp23/rdtsc_support.cpp -o example -lc++ -lc++abi"
cere configure \
  --build-cmd="${BUILD_CMD}" \
  --run-cmd="./example" \
  --clean-cmd="rm -f example"

cere profile

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
