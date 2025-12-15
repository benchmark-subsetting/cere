#!/bin/bash
exec ./test_dump_direct 2>&1 &
PID=$!
sleep 0.5

# Wait for tracer
TRACER_PID=$(pgrep -P $PID cere-tracer)
if [ -z "$TRACER_PID" ]; then
  echo "No tracer found"
  kill $PID 2>/dev/null
  exit 1
fi

echo "Found tracer PID: $TRACER_PID"
echo "Tracee PID: $PID"

# Wait a bit for the crash to happen
sleep 2

# Get register info from the tracee
echo ""
echo "=== Tracee registers at crash ==="
cat /proc/$PID/status | grep -E "(State|Pid|PPid)"

# Try to get the instruction pointer
if [ -f /proc/$PID/stat ]; then
  echo "Tracee still exists, getting stack trace..."
  gdb -batch -p $PID -ex "bt" -ex "info registers" -ex "x/10i \$rip" 2>&1 | head -50
fi

kill $PID $TRACER_PID 2>/dev/null
