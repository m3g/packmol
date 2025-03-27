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
FLAGS= -O3 -march=native -funroll-loops
SRCDIR= src
MAINDIR= app 
###################################################################
#                                                                 #
# Generally no modifications are required after this.             #
#                                                                 #
###################################################################
#
# Flags for compiling development version
#
GENCANFLAGS := $(FLAGS)
# Flags for the routines that signal with --fast-math
IEEE_SIGNAL_FLAGS := $(FLAGS)
ifeq ($(MAKECMDGOALS),devel)
FLAGS = -Wall -fcheck=bounds -g -fbacktrace -ffpe-trap=zero,overflow,underflow
GENCANFLAGS = -fcheck=bounds -g -fbacktrace -ffpe-trap=zero,overflow,underflow 
endif
ifeq ($(MAKECMDGOALS),perf)
FLAGS = -g -pg
GENCANFLAGS = -g -pg
endif
ifeq ($(MAKECMDGOALS),static)
FLAGS = -O3 --fast-math -static
GENCANFLAGS = -O3 --fast-math -static
endif
#
# Files required
#
oall = cenmass.o \
       gencan.o \
       pgencan.o \
       initial.o \
       title.o \
       setsizes.o \
       exit_codes.o \
       getinp.o \
       strlength.o \
       output.o \
       checkpoint.o \
       writesuccess.o \
       fparc.o \
       gparc.o \
       gwalls.o \
       comprest.o \
       comparegrad.o \
       packmol.o \
       polartocart.o \
       resetcells.o \
       tobar.o \
       cell_indexing.o \
       restmol.o \
       swaptype.o \
       swaptypemod.o \
       ahestetic.o \
       heuristics.o \
       flashsort.o \
       jacobi.o \
       random.o \
       sizes.o \
       pbc.o \
       usegencan.o \
       compute_data.o \
       flashmod.o \
       computef.o \
       computeg.o \
       input.o \
	   gencan_ieee_signal_routines.o
#
# Linking 
#
all : $(oall)
	@echo " ------------------------------------------------------ " 
	@echo " Compiling packmol with $(FORTRAN) " 
	@echo " Flags: $(FLAGS) " 
	@echo " ------------------------------------------------------ " 
	@$(FORTRAN) -o packmol $(oall) $(FLAGS) 
	@\rm -f *.mod *.o
	@echo " ------------------------------------------------------ " 
	@echo " Packmol succesfully built." 
	@echo " ------------------------------------------------------ " 
#
# Compiling with flags for development
#
static : devel
perf : devel
devel : $(oall)
	@echo " ------------------------------------------------------ " 
	@echo " Compiling packmol with $(FORTRAN) " 
	@echo " Flags: $(FLAGS)"
	@echo " ------------------------------------------------------ "
	@$(FORTRAN) -o packmol $(oall) $(FLAGS)
	@echo " ------------------------------------------------------ " 
	@echo " Packmol succesfully built. " 
	@echo " ------------------------------------------------------ " 
#
# Modules
#
modules = exit_codes.o sizes.o pbc.o compute_data.o usegencan.o input.o flashmod.o \
          swaptypemod.o ahestetic.o cell_indexing.o
exit_codes.o : $(SRCDIR)/exit_codes.f90
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/exit_codes.f90
sizes.o : $(SRCDIR)/sizes.f90 
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/sizes.f90
pbc.o : $(SRCDIR)/pbc.f90 
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/pbc.f90
compute_data.o : $(SRCDIR)/compute_data.f90 sizes.o
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/compute_data.f90
input.o : $(SRCDIR)/input.f90 sizes.o 
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/input.f90
flashmod.o : $(SRCDIR)/flashmod.f90 sizes.o 
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/flashmod.f90
usegencan.o : $(SRCDIR)/usegencan.f90 sizes.o
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/usegencan.f90
swaptypemod.o : $(SRCDIR)/swaptypemod.f90 
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/swaptypemod.f90
ahestetic.o : $(SRCDIR)/ahestetic.f90 
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/ahestetic.f90
cell_indexing.o : $(SRCDIR)/cell_indexing.f90
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/cell_indexing.f90

#
# Code compiled only for all versions
#
cenmass.o : $(SRCDIR)/cenmass.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/cenmass.f90
initial.o : $(SRCDIR)/initial.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/initial.f90
title.o : $(SRCDIR)/title.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/title.f90
setsizes.o : $(SRCDIR)/setsizes.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/setsizes.f90
getinp.o : $(SRCDIR)/getinp.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/getinp.f90
strlength.o : $(SRCDIR)/strlength.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/strlength.f90
output.o : $(SRCDIR)/output.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/output.f90
checkpoint.o : $(SRCDIR)/checkpoint.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/checkpoint.f90
writesuccess.o : $(SRCDIR)/writesuccess.f90  $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/writesuccess.f90
fparc.o : $(SRCDIR)/fparc.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/fparc.f90
gparc.o : $(SRCDIR)/gparc.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/gparc.f90
gwalls.o : $(SRCDIR)/gwalls.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/gwalls.f90
comprest.o : $(SRCDIR)/comprest.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/comprest.f90
comparegrad.o : $(SRCDIR)/comparegrad.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/comparegrad.f90
packmol.o : app/packmol.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c app/packmol.f90
polartocart.o : $(SRCDIR)/polartocart.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/polartocart.f90
resetcells.o : $(SRCDIR)/resetcells.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/resetcells.f90
tobar.o : $(SRCDIR)/tobar.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/tobar.f90
restmol.o : $(SRCDIR)/restmol.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/restmol.f90
swaptype.o : $(SRCDIR)/swaptype.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/swaptype.f90
heuristics.o : $(SRCDIR)/heuristics.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/heuristics.f90
flashsort.o : $(SRCDIR)/flashsort.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/flashsort.f90
jacobi.o : $(SRCDIR)/jacobi.f90
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/jacobi.f90
pgencan.o : $(SRCDIR)/pgencan.f90 $(modules)
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/pgencan.f90
random.o : $(SRCDIR)/random.f90 
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/random.f90
computef.o : $(SRCDIR)/computef.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/computef.f90
computeg.o : $(SRCDIR)/computeg.f90 $(modules)   
	@$(FORTRAN) $(FLAGS) -c $(SRCDIR)/computeg.f90
gencan_ieee_signal_routines.o : $(SRCDIR)/gencan_ieee_signal_routines.f90
	@$(FORTRAN) $(IEEE_SIGNAL_FLAGS) -c $(SRCDIR)/gencan_ieee_signal_routines.f90
gencan.o : $(SRCDIR)/gencan.f gencan_ieee_signal_routines.o
	@$(FORTRAN) $(GENCANFLAGS) -c $(SRCDIR)/gencan.f 
#
# Clean build files
#
clean: 
	@\rm -f ./*.o ./*.mod ./src/*.mod ./src/*.o
#
# Remove all build and executable files to upload to git
#
cleanall:  
	@\rm -f ./packmol ./*.o ./*.mod ./src/*.mod ./src/*.o
