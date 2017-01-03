./tests.verify: test performed on 2003.07.03 


#### Test: ./tests.verify running ./make_module_1.sh 
running build
running run_f2py
using F2PY 2.23.190-1369
f2py ../hw.f -m hw -h /tmp/@13028.0/hw.pyf --overwrite-signature
Reading fortran codes...
	Reading file '../hw.f'
Post-processing...
	Block: hw
			Block: hwtest
			Block: hw1
			Block: hw2
			Block: hw3_v1
			Block: hw3
			Block: hw4
Saving signatures to file "/tmp/@13028.0/hw.pyf"
f2py --build-dir /tmp/@13028.0 /tmp/@13028.0/hw.pyf
Reading fortran codes...
	Reading file '/tmp/@13028.0/hw.pyf'
Post-processing...
	Block: hw
			Block: hw1
			Block: hw2
			Block: hw3_v1
			Block: hw3
			Block: hw4
Building modules...
	Building module "hw"...
		Creating wrapper for Fortran function "hw1"("hw1")...
		Constructing wrapper function "hw1"...
		  hw1 = hw1(r1,r2)
		Constructing wrapper function "hw2"...
		  hw2(r1,r2)
		Constructing wrapper function "hw3_v1"...
		  hw3_v1(r1,r2,s)
		Constructing wrapper function "hw3"...
		  s = hw3(r1,r2)
		Constructing wrapper function "hw4"...
		  s = hw4(r1,r2)
	Wrote C/API module "hw" to file "/tmp/@13028.0/hwmodule.c"
	Fortran 77 wrappers are saved to "/tmp/@13028.0/hw-f2pywrappers.f"
running build_flib
running find_fortran_compiler
running gnu_fortran_compiler.find_lib_directories
g77 -v
detecting Gnu Fortran compiler...
g77 --version 
found GNU Fortran (GCC 3.2.3 20030331 (Debian prerelease)) 3.2.3 20030331 (prerelease)
using Gnu 3.2.3 Fortran compiler
 building 'hw' library
g77  -Wall -fno-second-underscore  -fPIC   -O3 -funroll-loops  -mmmx  -msse  -march=i686  -malign-double -fomit-frame-pointer  -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src  -c ../hw.f -o /tmp/@13028.0/hw.o
g77  -Wall -fno-second-underscore  -fPIC   -O3 -funroll-loops  -mmmx  -msse  -march=i686  -malign-double -fomit-frame-pointer  -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src  -c /tmp/@13028.0/hw-f2pywrappers.f -o /tmp/@13028.0/hw-f2pywrappers.o
ar -cur /tmp/@13028.0/libhw.a /tmp/@13028.0/hw.o /tmp/@13028.0/hw-f2pywrappers.o
ranlib  /tmp/@13028.0/libhw.a
running build_ext
replacing linker_so 'gcc -shared' with 'g77 -shared'
building 'hw' extension
gcc -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src -I/work/NO/ext/Linux/include/python2.2 -c /work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src/fortranobject.c -o /tmp/@13028.0/fortranobject.o
gcc -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src -I/work/NO/ext/Linux/include/python2.2 -c /tmp/@13028.0/hwmodule.c -o /tmp/@13028.0/hwmodule.o
g77 -shared /tmp/@13028.0/fortranobject.o /tmp/@13028.0/hwmodule.o -L/tmp/@13028.0 -lhw -lg2c-pic -o ./hw.so
restoring linker_so 'gcc -shared'
Removing build directory /tmp/@13028.0
This module 'hw' is auto-generated with f2py (version:2.23.190-1369).
Functions:
  hw1 = hw1(r1,r2)
  hw2(r1,r2)
  hw3_v1(r1,r2,s)
  s = hw3(r1,r2)
  s = hw4(r1,r2)
.
s should be 0 but is not... s = 10
CPU time of ./make_module_1.sh: 3.0 seconds on hpl-lapx30 i686, Linux


#### Test: ./tests.verify running ./hwa.py 1.2 -1.2
hw1, result: 0.0
hw2, result: Hello, World! sin(-2.000)=-.909503
 hw3, result: 0.0
CPU time of ./hwa.py: 0.1 seconds on hpl-lapx30 i686, Linux


#### Test: ./tests.verify running ./make_module_2.sh 
Reading fortran codes...
	Reading file '../hw.f'
Post-processing...
	Block: hw
			Block: hwtest
			Block: hw1
			Block: hw2
			Block: hw3_v1
			Block: hw3
			Block: hw4
Saving signatures to file "./hw.pyf"
real\*8\s*::\s*s replaced by real*8, intent(out) :: s in hw.pyf
running build
running run_f2py
using F2PY 2.23.190-1369
f2py --build-dir /tmp/@13063.0 hw.pyf
Reading fortran codes...
	Reading file 'hw.pyf'
Post-processing...
	Block: hw
			Block: hw1
			Block: hw2
			Block: hw3_v1
			Block: hw3
			Block: hw4
Building modules...
	Building module "hw"...
		Creating wrapper for Fortran function "hw1"("hw1")...
		Constructing wrapper function "hw1"...
		  hw1 = hw1(r1,r2)
		Constructing wrapper function "hw2"...
		  hw2(r1,r2)
		Constructing wrapper function "hw3_v1"...
		  s = hw3_v1(r1,r2)
		Constructing wrapper function "hw3"...
		  s = hw3(r1,r2)
		Constructing wrapper function "hw4"...
		  s = hw4(r1,r2)
	Wrote C/API module "hw" to file "/tmp/@13063.0/hwmodule.c"
	Fortran 77 wrappers are saved to "/tmp/@13063.0/hw-f2pywrappers.f"
running build_flib
running find_fortran_compiler
running gnu_fortran_compiler.find_lib_directories
g77 -v
detecting Gnu Fortran compiler...
g77 --version 
found GNU Fortran (GCC 3.2.3 20030331 (Debian prerelease)) 3.2.3 20030331 (prerelease)
using Gnu 3.2.3 Fortran compiler
 building 'hw' library
g77  -Wall -fno-second-underscore  -fPIC   -O3 -funroll-loops  -mmmx  -msse  -march=i686  -malign-double -fomit-frame-pointer  -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src  -c ../hw.f -o /tmp/@13063.0/hw.o
g77  -Wall -fno-second-underscore  -fPIC   -O3 -funroll-loops  -mmmx  -msse  -march=i686  -malign-double -fomit-frame-pointer  -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src  -c /tmp/@13063.0/hw-f2pywrappers.f -o /tmp/@13063.0/hw-f2pywrappers.o
ar -cur /tmp/@13063.0/libhw.a /tmp/@13063.0/hw.o /tmp/@13063.0/hw-f2pywrappers.o
ranlib  /tmp/@13063.0/libhw.a
running build_ext
replacing linker_so 'gcc -shared' with 'g77 -shared'
building 'hw' extension
gcc -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src -I/work/NO/ext/Linux/include/python2.2 -c /work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src/fortranobject.c -o /tmp/@13063.0/fortranobject.o
gcc -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -I/work/NO/ext/Linux/lib/python2.2/site-packages/f2py2e/src -I/work/NO/ext/Linux/include/python2.2 -c /tmp/@13063.0/hwmodule.c -o /tmp/@13063.0/hwmodule.o
g77 -shared /tmp/@13063.0/fortranobject.o /tmp/@13063.0/hwmodule.o -L/tmp/@13063.0 -lhw -lg2c-pic -o ./hw.so
restoring linker_so 'gcc -shared'
Removing build directory /tmp/@13063.0
0.0
This module 'hw' is auto-generated with f2py (version:2.23.190-1369).
Functions:
  hw1 = hw1(r1,r2)
  hw2(r1,r2)
  s = hw3_v1(r1,r2)
  s = hw3(r1,r2)
  s = hw4(r1,r2)
.
CPU time of ./make_module_2.sh: 3.2 seconds on hpl-lapx30 i686, Linux



----- appending file hw.pyf ------
!    -*- f90 -*-
python module hw ! in 
    interface  ! in :hw
        function hw1(r1,r2) ! in :hw:../hw.f
            real*8 :: r1
            real*8 :: r2
            real*8 :: hw1
        end function hw1
        subroutine hw2(r1,r2) ! in :hw:../hw.f
            real*8 :: r1
            real*8 :: r2
        end subroutine hw2
        subroutine hw3_v1(r1,r2,s) ! in :hw:../hw.f
            real*8 :: r1
            real*8 :: r2
            real*8, intent(out) :: s
        end subroutine hw3_v1
        subroutine hw3(r1,r2,s) ! in :hw:../hw.f
            real*8 :: r1
            real*8 :: r2
            real*8 intent(out) :: s
        end subroutine hw3
        subroutine hw4(r1,r2,s) ! in :hw:../hw.f
            real*8 :: r1
            real*8 :: r2
            real*8 intent(out) :: s
        end subroutine hw4
    end interface 
end python module hw

! This file was auto-generated with f2py (version:2.23.190-1369).
! See http://cens.ioc.ee/projects/f2py2e/

#### Test: ./tests.verify running ./hwa.py 1.2 -1.2
hw1, result: 0.0
hw2, result: Hello, World! sin(-2.000)=-.909503
 hw3, result: 0.0
CPU time of ./hwa.py: 0.1 seconds on hpl-lapx30 i686, Linux

