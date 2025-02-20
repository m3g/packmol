#!/bin/bash
echo "Running connectivity test..."

../packmol < ./input_files/benzene2.inp > /dev/null

if diff <(grep "^CONECT" ./output.pdb) <(grep "^CONECT" ./output_files/benzene2.pdb) >/dev/null; then
    exit 0  # Files are identical
else
    echo "Connectivity test failed."
    exit 1  # Files are different
fi