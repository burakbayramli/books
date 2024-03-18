FC = gfortran
executables := $(patsubst %.f,%.out,$(wildcard *.f))


all:
	make $(executables)

%.out: %.f
	$(FC) $*.f -o $*.out

clean:
	-rm *.out
