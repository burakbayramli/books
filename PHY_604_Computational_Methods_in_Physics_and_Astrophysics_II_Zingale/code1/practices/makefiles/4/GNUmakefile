# computer and compilation options
FCOMP := gfortran
DEBUG :=

# set the compiler flags depending on whether we are in DEBUG mode
ifdef DEBUG
  FFLAGS := -g -O0 -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -Wuninitialized
else
  FFLAGS := -O
endif

# list of source files
f90sources += main.f90 compute.f90 print_result.f90 githash.f90

# list of resulting object files
f90objects = $(f90sources:.f90=.o)

# main target to build
all: main

# inter-file dependencies
include GMake.inc


# githash.f90 is a special sourcefile that is created at compile time.
# note that we remove it on clean.
githash.f90:
	./makegithash.sh 


# rule to make a .o from a .f90 file
%.o: %.f90
	$(FCOMP) $(FFLAGS) -c $<

# rule to build the main target
main: $(f90objects)
	$(FCOMP) -o main $(f90objects)

# rule to remove intermediate .o and .mod files
clean: 
	rm -f *.o *.mod
	rm -f githash.f90

.PHONY: clean

# for debugging.  To see the value of a Makefile variable,
# e.g. FFLAGS, simply do "make print-FFLAGS".  This will
# print out the value.
print-%: ; @echo $* is $($*)
