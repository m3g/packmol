#!/bin/bash

# Fortran compiler to use
export FC=gfortran
# Compiler flags to use
export FFLAGS="-g -O2 -Wall"
#export FFLAGS="-g -O2 -Wall -fbounds-check"
#export FFLAGS="-g -O0 -Wall -fbounds-check"

# Installation directory
export target=$(pwd) # this installs packmol under bin/ in the present directory

# Number of parallel processes in build
export npar=4

# No changes should be necessary hereafter.
if [[ ! -d objdir ]]; then
    mkdir objdir
fi
cd objdir
cmake .. \
      -DCMAKE_INSTALL_PREFIX=${target} \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_Fortran_FLAGS_RELEASE:STRING="-DNDEBUG"
make -j ${npar} install VERBOSE=1
cd ..
