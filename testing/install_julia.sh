#!/bin/bash
# Install Julia
if [[ $(which juliaup) ]]; then
    echo "juliaup found"
else
    curl -fsSL https://install.julialang.org > juliaup.sh
    chmod +x juliaup.sh
    ./juliaup.sh -y
fi