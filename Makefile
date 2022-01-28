# Makefile for grid generation program
# Written by: Mark Herndon
# Lehigh University: Department of Mechanical
# Engineering and Mechanics

# Compiler (ifort, gfortran)
FC = ifort
CC = icc

export FC
export CC

MODDIR = .mod

ifneq ($(MODDIR),)
  $(shell test -d $(MODDIR) || mkdir -p $(MODDIR))
  FCFLAGS+= -module $(MODDIR)
endif

COMMONFLAGS = -r8 -traceback
PRODFLAGS = -O3


COMPFLAGS = ${COMMONFLAGS} ${PRODFLAGS}

export COMPFLAGS

# Executable name
EXEC_NAME = grid_generator.exe

# Object list
OBJECTS = mod_grid_setup.o \
	  main.o

%.o: %.f90; $(FC) $(COMPFLAGS) $(FCFLAGS) -c -o $@ $<

all: $(OBJECTS)
	$(FC) $(COMPFLAGS) $(FCFLAGS) -o $(EXEC_NAME) $(OBJECTS)

clean:
	rm -rf $(EXEC_NAME) .mod *.o
