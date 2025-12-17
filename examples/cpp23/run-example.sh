#!/bin/bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
# Allow overriding the build directory to avoid permission issues
BUILD="${CERE_BUILD_DIR:-$ROOT/examples/cpp23/build}"

mkdir -p "${BUILD}"
cd "${BUILD}"

# Work on local copies to avoid permissions issues in mounted volumes
cp -f "${ROOT}/examples/cpp23/example.cpp" .
cp -f "${ROOT}/examples/cpp23/rdtsc_support.cpp" .

export PYTHONPATH="${ROOT}/src:${PYTHONPATH:-/usr/local/lib/python3.12/site-packages}"
export PATH="${ROOT}/src:/usr/local/bin:${PATH}"
LIBDIR="${CERE_LIBDIR:-/usr/local/lib}"
export CERE_LIBDIR="${LIBDIR}"
export LD_LIBRARY_PATH="/usr/lib/x86_64-linux-gnu:/usr/lib/llvm-16/lib:${LIBDIR}:${LD_LIBRARY_PATH:-}"

# Avoid PIE so the replay linker can consume the captured symbol file
BUILD_CMD="cerec++ -g -std=gnu++20 -fno-pie -no-pie ${BUILD}/example.cpp ${BUILD}/rdtsc_support.cpp -o example -lstdc++"

cere configure \
  --build-cmd="${BUILD_CMD}" \
  --run-cmd="./example" \
  --clean-cmd="rm -f example"

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
