#!/bin/bash
echo "Running connectivity test..."

# Test 1
../packmol < ./input_files/benzene2.inp > /dev/null
if diff <(grep "^CONECT" ./output.pdb) <(grep "^CONECT" ./output_files/benzene2.pdb) >/dev/null; then
    exit 0  # Files are identical
else
    echo "Connectivity test failed (test 1)."
    exit 1  # Files are different
fi

# Test 2
../packmol < ./input_files/water_box_connect.inp > /dev/null
if diff <(grep "^CONECT" ./output.pdb) <(grep "^CONECT" ./output_files/water_connect.pdb) >/dev/null; then
    exit 0  # Files are identical
else
    echo "Connectivity test failed (test 2)."
    exit 1  # Files are different
fi
