cmake_minimum_required(VERSION 3.10)
project(packmol Fortran)

set(PACKMOL_PROFILE "baseline" CACHE STRING "Build profile: baseline, perf-native, debug, sanitize")
set_property(CACHE PACKMOL_PROFILE PROPERTY STRINGS baseline perf-native debug sanitize)
option(PACKMOL_UNSAFE_MATH "Enable unsafe/fast math optimizations" OFF)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    add_compile_options(-Wall)
endif()

if(PACKMOL_PROFILE STREQUAL "baseline")
    add_compile_options(-O2)
elseif(PACKMOL_PROFILE STREQUAL "perf-native")
    add_compile_options(-O3)
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        add_compile_options(-march=native -mtune=native -funroll-loops)
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES IntelLLVM OR CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        add_compile_options(-xHost -ipo)
    endif()
elseif(PACKMOL_PROFILE STREQUAL "debug")
    add_compile_options(-g)
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        add_compile_options(-fcheck=bounds -fbacktrace -ffpe-trap=zero,overflow,underflow)
    endif()
elseif(PACKMOL_PROFILE STREQUAL "sanitize")
    add_compile_options(-O1 -g)
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        add_compile_options(-fno-omit-frame-pointer -fsanitize=address,undefined -fcheck=all)
        add_link_options(-fsanitize=address,undefined)
    endif()
else()
    message(FATAL_ERROR "Unknown PACKMOL_PROFILE='${PACKMOL_PROFILE}'. Use one of: baseline, perf-native, debug, sanitize")
endif()

if(PACKMOL_UNSAFE_MATH)
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        add_compile_options(-ffast-math)
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES IntelLLVM OR CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        add_compile_options(-fp-model fast=2)
    endif()
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
       src/cli_parser.f90
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
