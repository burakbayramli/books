./tests.verify: test performed on 2004.03.31 


#### Test: ./tests.verify running ./make_F77_app.sh 
we make a test run:
 call gridloop1 was run 50 times!
 in f2  0.
 in f2  0.5
 in f2  1.
 in f2  1.
 in f2  1.5
 in f2  2.
 in f2  2.
 in f2  2.5
 in f2  3.
value at ( 0.000, 0.000) = a(  0,  0) =  0.00000E+00
value at ( 0.500, 0.000) = a(  1,  0) =  0.50000E+00
value at ( 1.000, 0.000) = a(  2,  0) =  0.10000E+01
value at ( 0.000, 0.500) = a(  0,  1) =  0.10000E+01
value at ( 0.500, 0.500) = a(  1,  1) =  0.15000E+01
value at ( 1.000, 0.500) = a(  2,  1) =  0.20000E+01
value at ( 0.000, 1.000) = a(  0,  2) =  0.20000E+01
value at ( 0.500, 1.000) = a(  1,  2) =  0.25000E+01
value at ( 1.000, 1.000) = a(  2,  2) =  0.30000E+01
cmd: ./tmp.app; elapsed=6.910000.2 cpu=6.860000.2
CPU time of ./make_F77_app.sh: 7.2 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running ./tmp.app 
 call gridloop1 was run 50 times!
 in f2  0.
 in f2  0.5
 in f2  1.
 in f2  1.
 in f2  1.5
 in f2  2.
 in f2  2.
 in f2  2.5
 in f2  3.
value at ( 0.000, 0.000) = a(  0,  0) =  0.00000E+00
value at ( 0.500, 0.000) = a(  1,  0) =  0.50000E+00
value at ( 1.000, 0.000) = a(  2,  0) =  0.10000E+01
value at ( 0.000, 0.500) = a(  0,  1) =  0.10000E+01
value at ( 0.500, 0.500) = a(  1,  1) =  0.15000E+01
value at ( 1.000, 0.500) = a(  2,  1) =  0.20000E+01
value at ( 0.000, 1.000) = a(  0,  2) =  0.20000E+01
value at ( 0.500, 1.000) = a(  1,  2) =  0.25000E+01
value at ( 1.000, 1.000) = a(  2,  2) =  0.30000E+01
CPU time of ./tmp.app: 6.9 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running ./make_module_1.sh 
running build
running run_f2py
creating tmp1
Reading fortran codes...
	Reading file 'gridloop.f'
Line #6 in gridloop.f:"      external f2, myfunc"
	analyzeline: ignoring program arguments
Line #6 in gridloop.f:"      external f2, myfunc"
	analyzeline: ignoring program arguments
Post-processing...
	Block: ext_gridloop
			Block: test
In: :ext_gridloop:gridloop.f:test
analyzevars:replacing parameter 'nmax' in 'nmax*nmax' (dimension of 'f') with '1100'
			Block: gridloop1
					Block: func1
			Block: gridloop2
					Block: func1
			Block: gridloop3
					Block: func1
			Block: gridloop4
					Block: func1
			Block: gridloop_vec
					Block: func1
			Block: gridloop_vec2
					Block: func1
			Block: gridloop2_str
			Block: gridloop1_v1
					Block: func1
			Block: gridloop1_v2
					Block: func1
			Block: gridloop1_v3
					Block: func1
			Block: gridloop1_v4
					Block: func1
			Block: gridloop1_v5
					Block: func1
			Block: transpose2dim
			Block: dump
			Block: change
			Block: gridloop2_fixedfunc1
			Block: myfunc
			Block: f2
Saving signatures to file "./tmp1/ext_gridloop.pyf"
Reading fortran codes...
	Reading file 'tmp1/ext_gridloop.pyf'
Post-processing...
	Block: gridloop1__user__routines
		Block: gridloop1_user_interface
			Block: func1
	Block: gridloop2__user__routines
		Block: gridloop2_user_interface
			Block: func1
	Block: gridloop3__user__routines
		Block: gridloop3_user_interface
			Block: func1
	Block: gridloop4__user__routines
		Block: gridloop4_user_interface
			Block: func1
	Block: gridloop_vec__user__routines
		Block: gridloop_vec_user_interface
			Block: func1
	Block: gridloop_vec2__user__routines
		Block: gridloop_vec2_user_interface
			Block: func1
	Block: gridloop1_v1__user__routines
		Block: gridloop1_v1_user_interface
			Block: func1
	Block: gridloop1_v2__user__routines
		Block: gridloop1_v2_user_interface
			Block: func1
	Block: gridloop1_v3__user__routines
		Block: gridloop1_v3_user_interface
			Block: func1
	Block: gridloop1_v4__user__routines
		Block: gridloop1_v4_user_interface
			Block: func1
	Block: gridloop1_v5__user__routines
		Block: gridloop1_v5_user_interface
			Block: func1
	Block: ext_gridloop
			Block: gridloop1
			Block: gridloop2
			Block: gridloop3
			Block: gridloop4
			Block: gridloop_vec
			Block: gridloop_vec2
			Block: gridloop2_str
			Block: gridloop1_v1
			Block: gridloop1_v2
			Block: gridloop1_v3
			Block: gridloop1_v4
			Block: gridloop1_v5
			Block: transpose2dim
			Block: dump
			Block: change
			Block: gridloop2_fixedfunc1
			Block: myfunc
			Block: f2
Building modules...
	Constructing call-back function "cb_func1_in_gridloop1__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop2__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop3__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop4__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop_vec__user__routines"
	  def func1(a,xcoor,ycoor,[nx,ny]): return a
	Constructing call-back function "cb_func1_in_gridloop_vec2__user__routines"
	  def func1(a,[nx,ny]): return a
	Constructing call-back function "cb_func1_in_gridloop1_v1__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop1_v2__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop1_v3__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop1_v4__user__routines"
	  def func1(x,y): return func1
	Constructing call-back function "cb_func1_in_gridloop1_v5__user__routines"
	  def func1(x,y): return func1
	Building module "ext_gridloop"...
		Constructing wrapper function "gridloop1"...
		  gridloop1(a,xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop2"...
		  a = gridloop2(xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop3"...
		  a = gridloop3(a,xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop4"...
		  a = gridloop4(a,xcoor,ycoor,func1,[nx,ny,overwrite_a,func1_extra_args])
		Constructing wrapper function "gridloop_vec"...
		  a = gridloop_vec(a,xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop_vec2"...
		  a = gridloop_vec2(a,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop2_str"...
		  a = gridloop2_str(xcoor,ycoor,func_str,[nx,ny])
		Constructing wrapper function "gridloop1_v1"...
		  gridloop1_v1(a,xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop1_v2"...
		  a = gridloop1_v2(xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop1_v3"...
		  gridloop1_v3(a,xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop1_v4"...
		  gridloop1_v4(a,xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "gridloop1_v5"...
		  gridloop1_v5(a,xcoor,ycoor,func1,[nx,ny,func1_extra_args])
		Constructing wrapper function "transpose2dim"...
		  transpose2dim(a,at,[nx,ny])
		Constructing wrapper function "dump"...
		  dump(a,xcoor,ycoor,[nx,ny])
		Constructing wrapper function "change"...
		  change(a,xcoor,ycoor,[nx,ny])
		Constructing wrapper function "gridloop2_fixedfunc1"...
		  a = gridloop2_fixedfunc1(xcoor,ycoor,[nx,ny])
		Creating wrapper for Fortran function "myfunc"("myfunc")...
		Constructing wrapper function "myfunc"...
		  myfunc = myfunc(x,y)
		Creating wrapper for Fortran function "f2"("f2")...
		Constructing wrapper function "f2"...
		  f2 = f2(x,y)
	Wrote C/API module "ext_gridloop" to file "tmp1/ext_gridloopmodule.c"
	Fortran 77 wrappers are saved to "tmp1/ext_gridloop-f2pywrappers.f"
running build_flib
running build_ext
building 'ext_gridloop' extension
creating tmp1/home
creating tmp1/home/hpl
creating tmp1/home/hpl/install
creating tmp1/home/hpl/install/lib
creating tmp1/home/hpl/install/lib/python
creating tmp1/home/hpl/install/lib/python/f2py2e
creating tmp1/home/hpl/install/lib/python/f2py2e/src
creating tmp1/tmp1
gcc -pthread -fno-strict-aliasing -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -DF2PY_REPORT_ON_ARRAY_COPY=1 -I/home/hpl/install/lib/python/f2py2e/src -I/usr/include/python2.3 -c /home/hpl/install/lib/python/f2py2e/src/fortranobject.c -o tmp1/home/hpl/install/lib/python/f2py2e/src/fortranobject.o
gcc -pthread -fno-strict-aliasing -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -DF2PY_REPORT_ON_ARRAY_COPY=1 -I/home/hpl/install/lib/python/f2py2e/src -I/usr/include/python2.3 -c tmp1/ext_gridloopmodule.c -o tmp1/tmp1/ext_gridloopmodule.o
g77 -shared tmp1/home/hpl/install/lib/python/f2py2e/src/fortranobject.o tmp1/tmp1/ext_gridloopmodule.o -Ltmp1 -lext_gridloop_f2py -lg2c-pic -o ./ext_gridloop.so
['__doc__', '__file__', '__name__', '__version__', 'as_column_major_storage', 'change', 'dump', 'f2', 'gridloop1', 'gridloop1_v1', 'gridloop1_v2', 'gridloop1_v3', 'gridloop1_v4', 'gridloop1_v5', 'gridloop2', 'gridloop2_fixedfunc1', 'gridloop2_str', 'gridloop3', 'gridloop4', 'gridloop_vec', 'gridloop_vec2', 'has_column_major_storage', 'myfunc', 'transpose2dim']
This module 'ext_gridloop' is auto-generated with f2py (version:2.39.235_1649).
Functions:
  gridloop1(a,xcoor,ycoor,func1,nx=len(xcoor),ny=len(ycoor),func1_extra_args=())
  a = gridloop2(xcoor,ycoor,func1,nx=len(xcoor),ny=len(ycoor),func1_extra_args=())
  a = gridloop3(a,xcoor,ycoor,func1,nx=shape(a,0),ny=shape(a,1),func1_extra_args=())
  a = gridloop4(a,xcoor,ycoor,func1,nx=shape(a,0),ny=shape(a,1),overwrite_a=1,func1_extra_args=())
  a = gridloop_vec(a,xcoor,ycoor,func1,nx=shape(a,0),ny=shape(a,1),func1_extra_args=())
  a = gridloop_vec2(a,func1,nx=shape(a,0),ny=shape(a,1),func1_extra_args=())
  a = gridloop2_str(xcoor,ycoor,func_str,nx=len(xcoor),ny=len(ycoor))
  gridloop1_v1(a,xcoor,ycoor,func1,nx=shape(a,0),ny=shape(a,1),func1_extra_args=())
  a = gridloop1_v2(xcoor,ycoor,func1,nx=len(xcoor),ny=len(ycoor),func1_extra_args=())
  gridloop1_v3(a,xcoor,ycoor,func1,nx=shape(a,0),ny=shape(a,1),func1_extra_args=())
  gridloop1_v4(a,xcoor,ycoor,func1,nx=shape(a,0),ny=shape(a,1),func1_extra_args=())
  gridloop1_v5(a,xcoor,ycoor,func1,nx=shape(a,0),ny=shape(a,1),func1_extra_args=())
  transpose2dim(a,at,nx=shape(a,0),ny=shape(a,1))
  dump(a,xcoor,ycoor,nx=shape(a,0),ny=shape(a,1))
  change(a,xcoor,ycoor,nx=shape(a,0),ny=shape(a,1))
  a = gridloop2_fixedfunc1(xcoor,ycoor,nx=len(xcoor),ny=len(ycoor))
  myfunc = myfunc(x,y)
  f2 = f2(x,y)
.
CPU time of ./make_module_1.sh: 9.1 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running ../Grid2Deff.py verify1
x+2*y = [[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
x+2*y = 0.0
x+2*y = 0.5
x+2*y = 1.0
x+2*y = 2.0
x+2*y = 2.5
x+2*y = 3.0
f computed by external gridloop1 function:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
x+2*y = 0.0
x+2*y = 0.5
x+2*y = 1.0
x+2*y = 2.0
x+2*y = 2.5
x+2*y = 3.0
f computed by external gridloop2 function:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
array seen from Python:
value at (0,0)  	 = a[0,0] = 0
value at (0,1)  	 = a[0,1] = 2
value at (0.5,0)  	 = a[1,0] = 0.5
value at (0.5,1)  	 = a[1,1] = 2.5
value at (1,0)  	 = a[2,0] = 1
value at (1,1)  	 = a[2,1] = 3
array seen from Fortran (transposed, but right values):
value at ( 0.000, 0.000) = a(  0,  0) =  0.00000E+00
value at ( 0.500, 0.000) = a(  1,  0) =  0.50000E+00
value at ( 1.000, 0.000) = a(  2,  0) =  0.10000E+01
value at ( 0.000, 1.000) = a(  0,  1) =  0.20000E+01
value at ( 0.500, 1.000) = a(  1,  1) =  0.25000E+01
value at ( 1.000, 1.000) = a(  2,  1) =  0.30000E+01
CPU time of ../Grid2Deff.py: 0.3 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running ../Grid2Deff.py verify2
running build
running run_f2py
creating tmp2
Reading fortran codes...
	Reading file '_cb.f'
Post-processing...
	Block: callback
			Block: fcb
			Block: gridloop2_fcb
Saving signatures to file "./tmp2/callback.pyf"
Reading fortran codes...
	Reading file 'tmp2/callback.pyf'
Post-processing...
	Block: callback
			Block: fcb
			Block: gridloop2_fcb
Building modules...
	Building module "callback"...
		Creating wrapper for Fortran function "fcb"("fcb")...
		Constructing wrapper function "fcb"...
		  fcb = fcb(x,y)
		Constructing wrapper function "gridloop2_fcb"...
		  a = gridloop2_fcb(xcoor,ycoor,[nx,ny])
	Wrote C/API module "callback" to file "tmp2/callbackmodule.c"
	Fortran 77 wrappers are saved to "tmp2/callback-f2pywrappers.f"
running build_flib
running build_ext
building 'callback' extension
creating tmp2/home
creating tmp2/home/hpl
creating tmp2/home/hpl/install
creating tmp2/home/hpl/install/lib
creating tmp2/home/hpl/install/lib/python
creating tmp2/home/hpl/install/lib/python/f2py2e
creating tmp2/home/hpl/install/lib/python/f2py2e/src
creating tmp2/tmp2
gcc -pthread -fno-strict-aliasing -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -DF2PY_REPORT_ON_ARRAY_COPY=1 -I/home/hpl/install/lib/python/f2py2e/src -I/usr/include/python2.3 -c tmp2/callbackmodule.c -o tmp2/tmp2/callbackmodule.o
gcc -pthread -fno-strict-aliasing -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -DF2PY_REPORT_ON_ARRAY_COPY=1 -I/home/hpl/install/lib/python/f2py2e/src -I/usr/include/python2.3 -c /home/hpl/install/lib/python/f2py2e/src/fortranobject.c -o tmp2/home/hpl/install/lib/python/f2py2e/src/fortranobject.o
g77 -shared tmp2/home/hpl/install/lib/python/f2py2e/src/fortranobject.o tmp2/tmp2/callbackmodule.o ./ext_gridloop.so -Ltmp2 -lcallback_f2py -lg2c-pic -o ./callback.so
running build
running run_f2py
Reading fortran codes...
	Reading file '_cb.f'
Post-processing...
	Block: ext_gridloop2
			Block: gridloop2
Saving signatures to file "./tmp1/ext_gridloop2.pyf"
Reading fortran codes...
	Reading file 'tmp1/ext_gridloop2.pyf'
Post-processing...
	Block: ext_gridloop2
			Block: gridloop2
Building modules...
	Building module "ext_gridloop2"...
		Constructing wrapper function "gridloop2"...
		  a = gridloop2(xcoor,ycoor,[nx,ny])
	Wrote C/API module "ext_gridloop2" to file "tmp1/ext_gridloop2module.c"
running build_flib
running build_ext
building 'ext_gridloop2' extension
gcc -pthread -fno-strict-aliasing -DNDEBUG -g -O3 -Wall -Wstrict-prototypes -fPIC -DF2PY_REPORT_ON_ARRAY_COPY=1 -I/home/hpl/install/lib/python/f2py2e/src -I/usr/include/python2.3 -c tmp1/ext_gridloop2module.c -o tmp1/tmp1/ext_gridloop2module.o
g77 -shared tmp1/home/hpl/install/lib/python/f2py2e/src/fortranobject.o tmp1/tmp1/ext_gridloop2module.o -Ltmp1 -lext_gridloop2_f2py -lg2c-pic -o ./ext_gridloop2.so
myfuncf77, a= [[ 0.          0.          0.          0.        ]
 [ 2.66666667  2.7775493   2.88706441  2.99386136]
 [ 5.33333333  5.55373108  5.7632897   5.95170314]
 [ 8.          8.3271947   8.6183698   8.84147098]]
g.ext_gridloop_vec(myfuncf77): a=
[[ 0.          0.          0.          0.        ]
 [ 2.66666667  2.7775493   2.88706441  2.99386136]
 [ 5.33333333  5.55373108  5.7632897   5.95170314]
 [ 8.          8.3271947   8.6183698   8.84147098]]
g.ext_gridloop_vec2(myfuncf772): a=
[[ 0.          0.          0.          0.        ]
 [ 2.66666667  2.7775493   2.88706441  2.99386136]
 [ 5.33333333  5.55373108  5.7632897   5.95170314]
 [ 8.          8.3271947   8.6183698   8.84147098]]
 in f2  0.
 in f2  0.333333333
 in f2  0.666666667
 in f2  1.
 in f2  0.666666667
 in f2  1.
 in f2  1.33333333
 in f2  1.66666667
 in f2  1.33333333
 in f2  1.66666667
 in f2  2.
 in f2  2.33333333
 in f2  2.
 in f2  2.33333333
 in f2  2.66666667
 in f2  3.
g.ext_gridloop2_str('f2'): a=
[[ 0.          0.66666667  1.33333333  2.        ]
 [ 0.33333333  1.          1.66666667  2.33333333]
 [ 0.66666667  1.33333333  2.          2.66666667]
 [ 1.          1.66666667  2.33333333  3.        ]]
g.ext_gridloop_str('myfunc'): a=
[[ 0.          0.          0.          0.        ]
 [ 2.66666667  2.7775493   2.88706441  2.99386136]
 [ 5.33333333  5.55373108  5.7632897   5.95170314]
 [ 8.          8.3271947   8.6183698   8.84147098]]
g.gridloop2_fcb: a=
[[ 0.          0.          0.          0.        ]
 [ 2.66666667  2.7775493   2.88706441  2.99386136]
 [ 5.33333333  5.55373108  5.7632897   5.95170314]
 [ 8.          8.3271947   8.6183698   8.84147098]]
contents of callback module: ['__doc__', '__file__', '__name__', '__version__', 'as_column_major_storage', 'fcb', 'gridloop2_fcb', 'has_column_major_storage']
g.gridloop2_v2: a=
[[ 0.          0.          0.          0.        ]
 [ 2.66666667  2.7775493   2.88706441  2.99386136]
 [ 5.33333333  5.55373108  5.7632897   5.95170314]
 [ 8.          8.3271947   8.6183698   8.84147098]]
CPU time of ../Grid2Deff.py: 4.1 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running ../Grid2Deff.py exceptions1
ext_gridloop.error failed in converting 1st argument `a' of ext_gridloop.gridloop1 to C/Fortran array
exceptions.TypeError ext_gridloop.gridloop2() argument 3 must be function, not str
exceptions.TypeError ext_gridloop.gridloop2() argument 3 must be function, not str
exceptions.TypeError ext_gridloop.gridloop2() argument 3 must be function, not str
CPU time of ../Grid2Deff.py: 0.3 seconds on hplx30 i686, Linux



----- appending file tmp1/ext_gridloop.pyf ------
!    -*- f90 -*-
python module gridloop1__user__routines 
    interface gridloop1_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop1:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop1_user_interface
end python module gridloop1__user__routines
python module gridloop2__user__routines 
    interface gridloop2_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop2:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop2_user_interface
end python module gridloop2__user__routines
python module gridloop3__user__routines 
    interface gridloop3_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop3:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop3_user_interface
end python module gridloop3__user__routines
python module gridloop4__user__routines 
    interface gridloop4_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop4:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop4_user_interface
end python module gridloop4__user__routines
python module gridloop_vec__user__routines 
    interface gridloop_vec_user_interface 
        subroutine func1(a,xcoor,ycoor,nx,ny) ! in :ext_gridloop:gridloop.f:gridloop_vec:unknown_interface
            real*8 dimension(nx,ny),intent(in,out) :: a
            real*8 dimension(nx),intent(in),depend(nx) :: xcoor
            real*8 dimension(ny),intent(in),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
        end subroutine func1
    end interface gridloop_vec_user_interface
end python module gridloop_vec__user__routines
python module gridloop_vec2__user__routines 
    interface gridloop_vec2_user_interface 
        subroutine func1(a,nx,ny) ! in :ext_gridloop:gridloop.f:gridloop_vec2:unknown_interface
            real*8 dimension(nx,ny),intent(in,out) :: a
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
        end subroutine func1
    end interface gridloop_vec2_user_interface
end python module gridloop_vec2__user__routines
python module gridloop1_v1__user__routines 
    interface gridloop1_v1_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop1_v1:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop1_v1_user_interface
end python module gridloop1_v1__user__routines
python module gridloop1_v2__user__routines 
    interface gridloop1_v2_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop1_v2:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop1_v2_user_interface
end python module gridloop1_v2__user__routines
python module gridloop1_v3__user__routines 
    interface gridloop1_v3_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop1_v3:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop1_v3_user_interface
end python module gridloop1_v3__user__routines
python module gridloop1_v4__user__routines 
    interface gridloop1_v4_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop1_v4:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop1_v4_user_interface
end python module gridloop1_v4__user__routines
python module gridloop1_v5__user__routines 
    interface gridloop1_v5_user_interface 
        function func1(x,y) ! in :ext_gridloop:gridloop.f:gridloop1_v5:unknown_interface
            real*8 :: x
            real*8 :: y
            real*8 :: func1
        end function func1
    end interface gridloop1_v5_user_interface
end python module gridloop1_v5__user__routines
python module ext_gridloop ! in 
    interface  ! in :ext_gridloop
        subroutine gridloop1(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop1__user__routines
            real*8 dimension(nx,ny),intent(inout),depend(nx,ny) :: a
            real*8 dimension(nx),intent(in) :: xcoor
            real*8 dimension(ny),intent(in) :: ycoor
            integer optional,check(len(xcoor)>=nx),depend(xcoor) :: nx=len(xcoor)
            integer optional,check(len(ycoor)>=ny),depend(ycoor) :: ny=len(ycoor)
            external func1
        end subroutine gridloop1
        subroutine gridloop2(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop2__user__routines
            real*8 dimension(nx,ny),intent(out),depend(nx,ny) :: a
            real*8 dimension(nx),intent(in) :: xcoor
            real*8 dimension(ny),intent(in) :: ycoor
            integer optional,check(len(xcoor)>=nx),depend(xcoor) :: nx=len(xcoor)
            integer optional,check(len(ycoor)>=ny),depend(ycoor) :: ny=len(ycoor)
            external func1
        end subroutine gridloop2
        subroutine gridloop3(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop3__user__routines
            real*8 dimension(nx,ny),intent(in,out) :: a
            real*8 dimension(nx),intent(in),depend(nx) :: xcoor
            real*8 dimension(ny),intent(in),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
        end subroutine gridloop3
        subroutine gridloop4(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop4__user__routines
            real*8 dimension(nx,ny),intent(in,out,overwrite) :: a
            real*8 dimension(nx),intent(in),depend(nx) :: xcoor
            real*8 dimension(ny),intent(in),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
        end subroutine gridloop4
        subroutine gridloop_vec(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop_vec__user__routines
            real*8 dimension(nx,ny),intent(in,out) :: a
            real*8 dimension(nx),intent(in),depend(nx) :: xcoor
            real*8 dimension(ny),intent(in),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
        end subroutine gridloop_vec
        subroutine gridloop_vec2(a,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop_vec2__user__routines
            real*8 dimension(nx,ny),intent(in,out) :: a
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
        end subroutine gridloop_vec2
        subroutine gridloop2_str(a,xcoor,ycoor,nx,ny,func_str) ! in :ext_gridloop:gridloop.f
            real*8 dimension(nx,ny),intent(out),depend(nx,ny) :: a
            real*8 dimension(nx),intent(in) :: xcoor
            real*8 dimension(ny),intent(in) :: ycoor
            integer optional,check(len(xcoor)>=nx),depend(xcoor) :: nx=len(xcoor)
            integer optional,check(len(ycoor)>=ny),depend(ycoor) :: ny=len(ycoor)
            character*(*) :: func_str
        end subroutine gridloop2_str
        subroutine gridloop1_v1(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop1_v1__user__routines
            real*8 dimension(nx,ny) :: a
            real*8 dimension(nx),depend(nx) :: xcoor
            real*8 dimension(ny),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
        end subroutine gridloop1_v1
        subroutine gridloop1_v2(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop1_v2__user__routines
            real*8 dimension(nx,ny),intent(out),depend(nx,ny) :: a
            real*8 dimension(nx),intent(in) :: xcoor
            real*8 dimension(ny),intent(in) :: ycoor
            integer optional,check(len(xcoor)>=nx),depend(xcoor) :: nx=len(xcoor)
            integer optional,check(len(ycoor)>=ny),depend(ycoor) :: ny=len(ycoor)
            external func1
        end subroutine gridloop1_v2
        subroutine gridloop1_v3(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop1_v3__user__routines
            real*8 dimension(nx,ny),intent(inout) :: a
            real*8 dimension(nx),depend(nx) :: xcoor
            real*8 dimension(ny),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
        end subroutine gridloop1_v3
        subroutine gridloop1_v4(a,xcoor,ycoor,nx,ny,func1) ! in :ext_gridloop:gridloop.f
            use gridloop1_v4__user__routines
            real*8 dimension(nx,ny),intent(inout,c) :: a
            real*8 dimension(nx),depend(nx) :: xcoor
            real*8 dimension(ny),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
        end subroutine gridloop1_v4
        subroutine gridloop1_v5(a,xcoor,ycoor,nx,ny,func1,at) ! in :ext_gridloop:gridloop.f
            use gridloop1_v5__user__routines
            real*8 dimension(nx,ny),intent(inout,c) :: a
            real*8 dimension(nx),depend(nx) :: xcoor
            real*8 dimension(ny),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
            external func1
            real*8 dimension(nx,ny),intent(in,hide),depend(nx,ny) :: at
        end subroutine gridloop1_v5
        subroutine transpose2dim(a,at,nx,ny) ! in :ext_gridloop:gridloop.f
            real*8 dimension(nx,ny) :: a
            real*8 dimension(ny,nx),depend(ny,nx) :: at
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
        end subroutine transpose2dim
        subroutine dump(a,xcoor,ycoor,nx,ny) ! in :ext_gridloop:gridloop.f
            real*8 dimension(nx,ny) :: a
            real*8 dimension(nx),depend(nx) :: xcoor
            real*8 dimension(ny),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
        end subroutine dump
        subroutine change(a,xcoor,ycoor,nx,ny) ! in :ext_gridloop:gridloop.f
            real*8 dimension(nx,ny) :: a
            real*8 dimension(nx),depend(nx) :: xcoor
            real*8 dimension(ny),depend(ny) :: ycoor
            integer optional,check(shape(a,0)==nx),depend(a) :: nx=shape(a,0)
            integer optional,check(shape(a,1)==ny),depend(a) :: ny=shape(a,1)
        end subroutine change
        subroutine gridloop2_fixedfunc1(a,xcoor,ycoor,nx,ny) ! in :ext_gridloop:gridloop.f
            real*8 dimension(nx,ny),intent(out),depend(nx,ny) :: a
            real*8 dimension(nx),intent(in) :: xcoor
            real*8 dimension(ny),intent(in) :: ycoor
            integer optional,check(len(xcoor)>=nx),depend(xcoor) :: nx=len(xcoor)
            integer optional,check(len(ycoor)>=ny),depend(ycoor) :: ny=len(ycoor)
        end subroutine gridloop2_fixedfunc1
        function myfunc(x,y) ! in :ext_gridloop:gridloop.f
            real*8 :: x
            real*8 :: y
            real*8 :: myfunc
        end function myfunc
        function f2(x,y) ! in :ext_gridloop:gridloop.f
            real*8 :: x
            real*8 :: y
            real*8 :: f2
        end function f2
    end interface 
end python module ext_gridloop

! This file was auto-generated with f2py (version:2.39.235_1649).
! See http://cens.ioc.ee/projects/f2py2e/
