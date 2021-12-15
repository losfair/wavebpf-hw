#!/bin/bash

set -e

run() {

stack ghci --main-is wbcore:exe:clashi << EOF
:load Wbcore.Tests.PriorityQueue
System.IO.hPutStrLn System.IO.stderr "Test: Wbcore.Tests.PriorityQueue"
Wbcore.Tests.PriorityQueue.run
System.IO.hPutStrLn System.IO.stderr "All tests completed"
EOF

}

cd "$(dirname $0)/.."
run 2>&1 > test_stdout.log | tee test_stderr.log

echo "-------------------"
echo "Checking result"
grep "All tests completed" test_stderr.log > /dev/null || (echo "Test did not complete" && exit 1)
grep "not equal" test_stderr.log > /dev/null && (echo "Found errors" && exit 1)
echo "Test passed"
