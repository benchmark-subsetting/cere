#!/bin/bash
set -e

cd "$(dirname "$0")/build"

# Use fixed config
cp ../cere_fixed.json cere.json

# Run CERE workflow
python3 -m cere configure --build-cmd="cerec++ -g -std=gnu++20 /workspace/examples/cpp23/example_fixed.cpp /workspace/examples/cpp23/rdtsc_support.cpp -o example_fixed -lstdc++" \
                          --run-cmd="./example_fixed" \
                          --clean-cmd="rm -f example_fixed"

python3 -m cere profile

# Check if CERE_REGION is set
if [ -n "$CERE_REGION" ]; then
  echo "Capturing region: $CERE_REGION"
  python3 -m cere capture --region="$CERE_REGION" --invocation="${CERE_INVOCATION:-1}"

  if [ -d ".cere/dumps/$CERE_REGION" ]; then
    echo "SUCCESS: Dump created at .cere/dumps/$CERE_REGION"
    ls -la ".cere/dumps/$CERE_REGION/"
  else
    echo "ERROR: No dump created"
    exit 1
  fi
else
  echo "Skipping capture/replay: set CERE_REGION to the desired loop name (see .cere/report after profiling)"
fi
