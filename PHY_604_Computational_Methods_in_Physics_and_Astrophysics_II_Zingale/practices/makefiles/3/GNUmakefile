# computer and compilation options
FCOMP := gfortran
FFLAGS := -O

# list of source files
f90sources += main.f90 compute.f90 print_result.f90

# list of resulting object files
f90objects = $(f90sources:.f90=.o)

# main target to build
all: main

# inter-file dependencies
main.o: compute.o
print_result.o: compute.o

# a pattern rule to make a .o from a .f90 file
%.o: %.f90
	$(FCOMP) $(FFLAGS) -c $<

# rule to build the main target
main: $(f90objects)
	$(FCOMP) -o main $(f90objects)

# rule to remove intermediate .o and .mod files
clean: 
	rm -f *.o *.mod

.PHONY: clean
