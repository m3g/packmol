cmake_minimum_required(VERSION 2.6)
project(packmol Fortran)


if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif(NOT CMAKE_BUILD_TYPE)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    add_compile_options(
        -Wall "$<$<CONFIG:Debug>:-Werror>"
    )
endif()

# Build the executable
add_executable(packmol
       src/exit_codes.f90
       src/cenmass.f90
       src/gencan.f
       src/gencan_ieee_signal_routines.f90
       src/pgencan.f90
       src/initial.f90
       src/title.f90
       src/setsizes.f90
       src/getinp.f90
       src/strlength.f90
       src/output.f90
       src/checkpoint.f90
       src/writesuccess.f90
       src/fparc.f90
       src/gparc.f90
       src/gwalls.f90
       src/comprest.f90
       src/comparegrad.f90
       app/packmol.f90
       src/polartocart.f90
       src/resetcells.f90
       src/tobar.f90
       src/cell_indexing.f90
       src/restmol.f90
       src/swaptype.f90
       src/swaptypemod.f90
       src/ahestetic.f90
       src/heuristics.f90
       src/flashsort.f90
       src/jacobi.f90
       src/random.f90
       src/sizes.f90
       src/pbc.f90
       src/usegencan.f90
       src/compute_data.f90
       src/flashmod.f90
       src/computef.f90
       src/computeg.f90
       src/input.f90
)

# Installation directive
install(TARGETS packmol DESTINATION bin)
