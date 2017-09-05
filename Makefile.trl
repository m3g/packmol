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
FORTRAN=/usr/bin/gfortran
#
# Change the flags of the compilation if you want:
#
FLAGS= -O3 --fast-math
 
###################################################################
#                                                                 #
# Generally no modifications are required after this.             #
#                                                                 #
###################################################################
#
# Flags for compiling development version
#
GENCANFLAGS := $(FLAGS)
ifeq ($(MAKECMDGOALS),devel)
FLAGS = -Wall -fcheck=bounds -g -fbacktrace -ffpe-trap=zero,overflow,underflow
GENCANFLAGS = -fcheck=bounds -g -fbacktrace -ffpe-trap=zero,overflow,underflow 
endif
ifeq ($(MAKECMDGOALS),perf)
FLAGS = -g -pg
GENCANFLAGS = -g -pg
endif
#
# Files required
#
# source files and objects
SRC_FILES=$(wildcard *.f*)
OBJ_FILES=$(patsubst %.f90, %.o, $(wildcard *.f90)) \
          $(patsubst %.f,   %.o, $(wildcard *.f))


#
## all       : Linking 
#
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
#
## devel     : Compiling with flags for development
#
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
	@$(FORTRAN) $(FLAGS) -c $<
compute_data.o : compute_data.f90 sizes.o
	@$(FORTRAN) $(FLAGS) -c $<
input.o : input.f90 sizes.o 
	@$(FORTRAN) $(FLAGS) -c $<
flashmod.o : flashmod.f90 sizes.o 
	@$(FORTRAN) $(FLAGS) -c $<
usegencan.o : usegencan.f90 sizes.o
	@$(FORTRAN) $(FLAGS) -c $<
swaptypemod.o : swaptypemod.f90 
	@$(FORTRAN) $(FLAGS) -c $<
ahestetic.o : ahestetic.f90 
	@$(FORTRAN) $(FLAGS) -c $<
#
# Code compiled only for all versions
#
%.o : %.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $<
gencan.o : gencan.f
	@$(FORTRAN) $(GENCANFLAGS) -c $<

#
## clean     : Clean build files
#
clean: 
	@\rm -f ./*.o ./*.mod 
#
## cleanall  : Remove all build and executable files to upload to git
#
cleanall:  
	@\rm -f ./packmol ./*.o ./*.mod

## variables : Print variables.
.PHONY : variables
variables :
	@echo SRC_FILES: $(SRC_FILES)
	@echo OBJ_FILES: $(OBJ_FILES)

.PHONY : help
help : Makefile.trl
	@sed -n 's/^## //p' $<
