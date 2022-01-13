# by default, make will try to build the first target it encounters.
# here we make up a dummy name "ALL" (note: this is not a special make
# name, it is just commonly used).
ALL: main

# explicitly list the rules for making each of the object files
main.o:
	gfortran -c main.f90

compute.o: compute.f90
	gfortran -c compute.f90

print_result.o: print_result.f90
	gfortran -c print_result.f90


# explicitly write the rule for linking together the executable
main: main.o compute.o print_result.o
	gfortran -o main main.o compute.o print_result.o
