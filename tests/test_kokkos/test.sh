#!/bin/bash
set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../../" && pwd)
BUILD="${ROOT}/examples/kokkos/build"

cd "${ROOT}"
rm -rf "${BUILD}/.cere"

./examples/kokkos/run-example.sh

GRAPH="${BUILD}/.cere/profile/graph_.dot"
if ! grep -q "__cere__kokkos_kernel" "${GRAPH}"; then
  echo "Expected __cere__kokkos_kernel in ${GRAPH}" >&2
  exit 1
fi

echo "kokkos example produced __cere__kokkos_kernel"
