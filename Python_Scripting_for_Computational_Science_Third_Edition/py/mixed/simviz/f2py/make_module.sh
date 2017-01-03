#!/bin/sh -x
f2py -m oscillator -c --build-dir tmp1 --fcompiler=gfortran \
     ../timeloop2.f $scripting/src/app/oscillator/F77/oscillator.f \
     only: scan2 timeloop2  :

python -c 'import oscillator; \
           print oscillator.__doc__; \
           print oscillator.timeloop2.__doc__; \
	   print oscillator.scan2.__doc__'

