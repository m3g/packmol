# configure generated Makefile
#
# Makefile for Packmol: Read the comments if you have some
#                       problem while compiling.
#
# You may use the ./configure script to search automatically for
# some fortran compiler.
#
# This make file will try to compile packmol with the default
# fortran compiler, defined by the FC directive. For doing this,
# just type
#
#          make 
#
# If you want to compile with some specific fortran compiler, you must 
# change the line below to the path of your fortran compiler. 
#
FORTRAN = /usr/bin/gfortran
#
# Change "AUTO" to the fortran command you want. 
#
# Change the flags of the compilation if you want:
#
FLAGS= -O3 -ffast-math 
 
###################################################################
#                                                                 #
# Generally no modifications are required after this.             #
#                                                                 #
###################################################################
#
# Get the default fortran compiler
#
ifeq ($(FORTRAN),AUTO)
FORTRAN = $(FC)
endif 
#
# Flags for compiling development version
#
GENCANFLAGS := $(FLAGS)
ifeq ($(MAKECMDGOALS),devel)
FLAGS = -Wall -fcheck=bounds
endif
#
# Files required
#
# source files and objects
SRC_FILES=$(wildcard *.f*)
OBJ_FILES=$(patsubst %.f90, %.o, $(wildcard *.f90)) \
          $(patsubst %.f,   %.o, $(wildcard *.f))


## all       : Linking 
all : $(OBJ_FILES)
	@echo " ------------------------------------------------------ " 
	@echo " Compiling packmol with $(FORTRAN) " 
	@echo " Flags: $(FLAGS) " 
	@echo " ------------------------------------------------------ " 
	@$(FORTRAN) -o packmol $(OBJ_FILES) $(FLAGS) 
	@\rm -f *.mod *.o
	@echo " ------------------------------------------------------ " 
	@echo " Packmol succesfully built." 
	@echo " ------------------------------------------------------ " 

## devel     : Compiling with flags for development
devel : $(OBJ_FILES)
	@echo " ------------------------------------------------------ " 
	@echo " Compiling packmol with $(FORTRAN) " 
	@echo " Flags: $(FLAGS)"
	@echo " ------------------------------------------------------ "
	@$(FORTRAN) -o packmol $(OBJ_FILES) $(FLAGS)
	@echo " ------------------------------------------------------ " 
	@echo " Packmol succesfully built. " 
	@echo " ------------------------------------------------------ " 
#
# Modules
#
modules = sizes.o compute_data.o usegencan.o input.o flashmod.o swaptypemod.o ahestetic.o
sizes.o : sizes.f90 
	@$(FORTRAN) $(FLAGS) -c sizes.f90
compute_data.o : compute_data.f90 sizes.o
	@$(FORTRAN) $(FLAGS) -c compute_data.f90
input.o : input.f90 sizes.o 
	@$(FORTRAN) $(FLAGS) -c input.f90
flashmod.o : flashmod.f90 sizes.o 
	@$(FORTRAN) $(FLAGS) -c flashmod.f90
usegencan.o : usegencan.f90 sizes.o
	@$(FORTRAN) $(FLAGS) -c usegencan.f90
swaptypemod.o : swaptypemod.f90 
	@$(FORTRAN) $(FLAGS) -c swaptypemod.f90
ahestetic.o : ahestetic.f90 
	@$(FORTRAN) $(FLAGS) -c ahestetic.f90
#
# Code compiled only for all versions
#
cenmass.o : cenmass.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c cenmass.f90
initial.o : initial.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c initial.f90
title.o : title.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c title.f90
setsizes.o : setsizes.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c setsizes.f90
getinp.o : getinp.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c getinp.f90
strlength.o : strlength.f90  
	@$(FORTRAN) $(FLAGS) -c strlength.f90
output.o : output.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c output.f90
checkpoint.o : checkpoint.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c checkpoint.f90
writesuccess.o : writesuccess.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c writesuccess.f90
fparc.o : fparc.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c fparc.f90
gparc.o : gparc.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c gparc.f90
gwalls.o : gwalls.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c gwalls.f90
comprest.o : comprest.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c comprest.f90
comparegrad.o : comparegrad.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c comparegrad.f90
packmol.o : packmol.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c packmol.f90
polartocart.o : polartocart.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c polartocart.f90
resetboxes.o : resetboxes.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c resetboxes.f90
tobar.o : tobar.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c tobar.f90
setibox.o : setibox.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c setibox.f90
restmol.o : restmol.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c restmol.f90
swaptype.o : swaptype.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c swaptype.f90
heuristics.o : heuristics.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c heuristics.f90
flashsort.o : flashsort.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c flashsort.f90
jacobi.o : jacobi.f90
	@$(FORTRAN) $(FLAGS) -c jacobi.f90
pgencan.o : pgencan.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c pgencan.f90
gencan.o : gencan.f
	@$(FORTRAN) $(GENCANFLAGS) -c gencan.f 
random.o : random.f90 
	@$(FORTRAN) $(FLAGS) -c random.f90
computef.o : computef.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c computef.f90
computeg.o : computeg.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c computeg.f90

## clean     : Clean build files
clean: 
	@\rm -f ./*.o ./*.mod 

## cleanall  : Remove all build and executable files to upload to git
cleanall:  
	@\rm -f ./packmol ./*.o ./*.mod

## variables : Print variables.
.PHONY : variables
variables :
	@echo SRC_FILES: $(SRC_FILES)
	@echo OBJ_FILES: $(OBJ_FILES)

.PHONY : help
help : Makefile
	@sed -n 's/^## //p' $<
