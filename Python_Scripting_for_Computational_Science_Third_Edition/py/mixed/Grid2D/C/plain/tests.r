./tests.verify: test performed on 2007.09.05 


#### Test: ./tests.verify running ./make_module_1.sh 
['__doc__', '__file__', '__name__', 'gridloop1', 'gridloop2']
module ext_gridloop:
   gridloop1(a, xcoor, ycoor, pyfunc)
   a = gridloop2(xcoor, ycoor, pyfunc)
CPU time of ./make_module_1.sh: 1.5 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running ../../Grid2Deff.py verify1
x+2*y = [[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
x+2*y = 0.0
x+2*y = 2.0
x+2*y = 0.5
x+2*y = 2.5
x+2*y = 1.0
x+2*y = 3.0
f computed by external gridloop1 function and f1:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
x+2*y = 0.0
x+2*y = 2.0
x+2*y = 0.5
x+2*y = 2.5
x+2*y = 1.0
x+2*y = 3.0
f computed by external gridloop2 function and f1:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
f computed by external gridloop1 function and StringFunction:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
f computed by external gridloop2 function and StringFunction:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
f computed by external gridloop2 function and StringFunction.__call__:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
f computed by __call__ and StringFunction:
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
CPU time of ../../Grid2Deff.py: 0.6 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running ../../Grid2Deff.py exceptions1
<type 'exceptions.TypeError'> gridloop1() argument 1 must be numpy.ndarray, not tuple
<type 'exceptions.ValueError'> a array is 1-dimensional or not of type float
<type 'exceptions.TypeError'> func1 is not a callable function
<type 'exceptions.NameError'> global name 'Complex64' is not defined
<type 'exceptions.ValueError'> xcoor array is 2-dimensional, but expected to be 1-dimensional
CPU time of ../../Grid2Deff.py: 0.6 seconds on ubuntu i686, Linux

