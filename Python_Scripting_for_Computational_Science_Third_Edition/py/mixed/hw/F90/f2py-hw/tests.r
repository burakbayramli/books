./tests.verify: test performed on 2010.05.25 


#### Test: ./tests.verify running ./make_module.sh 
Unknown vendor: "gfortran"
running build
running config_cc
unifing config_cc, config, build_clib, build_ext, build commands --compiler options
running config_fc
unifing config_fc, config, build_clib, build_ext, build commands --fcompiler options
running build_src
build_src
building extension "hw" sources
f2py options: []
f2py:> /tmp/tmpIGuZbf/src.linux-i686-2.6/hwmodule.c
creating /tmp/tmpIGuZbf
creating /tmp/tmpIGuZbf/src.linux-i686-2.6
Reading fortran codes...
	Reading file '../hw.f90' (format:free)
Post-processing...
	Block: hw
			Block: hwtest
			Block: hw1
			Block: hw2
			Block: hw3
Post-processing (stage 2)...
Building modules...
	Building module "hw"...
		Creating wrapper for Fortran function "hw1"("hw1")...
		Constructing wrapper function "hw1"...
		  hw1 = hw1(r1,r2)
		Constructing wrapper function "hw2"...
		  hw2(r1,r2)
		Constructing wrapper function "hw3"...
		  s = hw3(r1,r2)
	Wrote C/API module "hw" to file "/tmp/tmpIGuZbf/src.linux-i686-2.6/hwmodule.c"
	Fortran 77 wrappers are saved to "/tmp/tmpIGuZbf/src.linux-i686-2.6/hw-f2pywrappers.f"
  adding '/tmp/tmpIGuZbf/src.linux-i686-2.6/fortranobject.c' to sources.
  adding '/tmp/tmpIGuZbf/src.linux-i686-2.6' to include_dirs.
copying /home/hpl/sysdir/Linux/lib/python2.6/site-packages/numpy/f2py/src/fortranobject.c -> /tmp/tmpIGuZbf/src.linux-i686-2.6
copying /home/hpl/sysdir/Linux/lib/python2.6/site-packages/numpy/f2py/src/fortranobject.h -> /tmp/tmpIGuZbf/src.linux-i686-2.6
  adding '/tmp/tmpIGuZbf/src.linux-i686-2.6/hw-f2pywrappers.f' to sources.
build_src: building npy-pkg config files
running build_ext
customize UnixCCompiler
customize UnixCCompiler using build_ext
customize Gnu95FCompiler
Found executable /usr/bin/gfortran
customize Gnu95FCompiler using build_ext
building 'hw' extension
compiling C sources
C compiler: gcc -pthread -fno-strict-aliasing -DNDEBUG -g -fwrapv -O3 -Wall -Wstrict-prototypes -fPIC

creating /tmp/tmpIGuZbf/tmp
creating /tmp/tmpIGuZbf/tmp/tmpIGuZbf
creating /tmp/tmpIGuZbf/tmp/tmpIGuZbf/src.linux-i686-2.6
compile options: '-I/tmp/tmpIGuZbf/src.linux-i686-2.6 -I/home/hpl/sysdir/Linux/lib/python2.6/site-packages/numpy/core/include -I/home/hpl/sysdir/Linux/include/python2.6 -c'
gcc: /tmp/tmpIGuZbf/src.linux-i686-2.6/hwmodule.c
gcc: /tmp/tmpIGuZbf/src.linux-i686-2.6/fortranobject.c
compiling Fortran sources
Fortran f77 compiler: /usr/bin/gfortran -Wall -ffixed-form -fno-second-underscore -fPIC -O3 -funroll-loops
Fortran f90 compiler: /usr/bin/gfortran -Wall -fno-second-underscore -fPIC -O3 -funroll-loops
Fortran fix compiler: /usr/bin/gfortran -Wall -ffixed-form -fno-second-underscore -Wall -fno-second-underscore -fPIC -O3 -funroll-loops
creating /tmp/tmpIGuZbf/F90
compile options: '-I/tmp/tmpIGuZbf/src.linux-i686-2.6 -I/home/hpl/sysdir/Linux/lib/python2.6/site-packages/numpy/core/include -I/home/hpl/sysdir/Linux/include/python2.6 -c'
gfortran:f90: ../hw.f90
gfortran:f77: /tmp/tmpIGuZbf/src.linux-i686-2.6/hw-f2pywrappers.f
/usr/bin/gfortran -Wall -Wall -shared /tmp/tmpIGuZbf/tmp/tmpIGuZbf/src.linux-i686-2.6/hwmodule.o /tmp/tmpIGuZbf/tmp/tmpIGuZbf/src.linux-i686-2.6/fortranobject.o /tmp/tmpIGuZbf/F90/hw.o /tmp/tmpIGuZbf/tmp/tmpIGuZbf/src.linux-i686-2.6/hw-f2pywrappers.o -lgfortran -o ./hw.so
running scons
Removing build directory /tmp/tmpIGuZbf
This module 'hw' is auto-generated with f2py (version:2_8079).
Functions:
  hw1 = hw1(r1,r2)
  hw2(r1,r2)
  s = hw3(r1,r2)
.
hw1, result: 0.0
hw2, result:  hw3, result: 0.0
 Hello, World! sin(   0.0000000000000000      )=   0.0000000000000000     
CPU time of ./make_module.sh: 2.0 seconds on hpl-laptop i686, Linux


#### Test: ./tests.verify running ./hwa.py 1.2 -1.2
hw1, result: 0.0
hw2, result:  hw3, result: 0.0
 Hello, World! sin(   0.0000000000000000      )=   0.0000000000000000     
CPU time of ./hwa.py: 0.1 seconds on hpl-laptop i686, Linux

