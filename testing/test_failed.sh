#!/bin/bash
#
# First argument: input file name
# Second argument: log file name
#
# Third argument: string to be searched in the log file, to declare that
# the test passed
#
# Example: ./test.sh "water_box.inp" "packmol.log" "FORCED"
#
if ! [ -f $1 ]; then
    echo "Error: input file not found: $1"
    exit 1
fi
../packmol < $1 > $2
if ! [ -f $2 ]; then
    echo "Error: Log file not generated: $2"
    exit 1
fi
if ! grep -q $3 $2; then
    echo "Error: could not find $3 in $2"
    exit 1
fi
echo "Test $1 OK."
exit 0
