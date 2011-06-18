#!/bin/bash

FAILED=0
for i in *.in
do
  echo $i
  ../../proto t < $i
  if [[ "$?" != "0" ]]; then
    FAILED=`echo $FAILED+1 | bc`
    FAILED_TESTS="$FAILED_TESTS\\n$i"
  fi
done

echo "$FAILED: tests failed"
echo "failing tests:\\n$FAILED_TESTS"
