./tests.verify: test performed on 2003.08.30 


#### Test: ./tests.verify running ./make_module_1.sh 
['__doc__', '__file__', '__name__', 'gridloop1', 'gridloop2']
CPU time of ./make_module_1.sh: 0.5 seconds on hpl-lapx30 i686, Linux


#### Test: ./tests.verify running ../Grid2Deff.py verify1
f1...x-y= [[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f1...x-y= 0.0
f1...x-y= 2.0
f1...x-y= 0.5
f1...x-y= 2.5
f1...x-y= 1.0
f1...x-y= 3.0
f computed by external gridloop1 function:
[[ 0.   2. ]
 [ 0.5  2.5]
 [ 1.   3. ]]
f is correct
f1...x-y= 0.0
f1...x-y= 2.0
f1...x-y= 0.5
f1...x-y= 2.5
f1...x-y= 1.0
f1...x-y= 3.0
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
CPU time of ../Grid2Deff.py: 0.2 seconds on hpl-lapx30 i686, Linux


#### Test: ./tests.verify running ../Grid2Deff.py exceptions1
exceptions.TypeError gridloop1() argument 1 must be array, not tuple
exceptions.ValueError a array is 1-dimensional or not of type Float
exceptions.TypeError func1 is not a callable function
exceptions.TypeError xcoor array is not of correct type (9)
exceptions.ValueError xcoor array is 2-dimensional, but expected to be 1-dimensional
CPU time of ../Grid2Deff.py: 0.2 seconds on hpl-lapx30 i686, Linux

