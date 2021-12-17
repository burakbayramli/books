FFLAGS := -g -O0 -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -Wuninitialized

all: test1 test2 test3

test1: test1.f90
	 gfortran ${FFLAGS} -o test1 test1.f90

test2: test2.f90
	 gfortran ${FFLAGS} -o test2 test2.f90

test3: test3.f90
	 gfortran ${FFLAGS} -o test3 test3.f90


clean:
	rm -f test1 test2 test3
