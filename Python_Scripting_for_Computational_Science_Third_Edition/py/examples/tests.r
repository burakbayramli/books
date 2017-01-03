./tests.verify: test performed on 2006.01.24 


#### Test: ./tests.verify running Grid2D.py 

x: [ 0.   0.5  1. ]
y: [ 0.   0.5  1. ] [[ 0.   1.   2. ]
 [ 0.5  1.5  2.5]
 [ 1.   2.   3. ]]

x: [ 0.   0.5  1. ]
y: [ 0.   0.5  1. ] [[ 0.   1.   2. ]
 [ 0.5  1.5  2.5]
 [ 1.   2.   3. ]]
CPU time of Grid2D.py: 0.4 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running Grid2Dit.py _verify1_Grid2Dit
interior point (1,1): (1,1)
boundary point (2,1): (2,1)
boundary point (1,2): (1,2)
boundary point (0,1): (0,1)
boundary point (1,0): (1,0)
corner point (0,0): (0,0)
corner point (2,0): (2,0)
corner point (2,2): (2,2)
corner point (0,2): (0,2)
any grid point (0,0): (0,0)
any grid point (1,0): (1,0)
any grid point (2,0): (2,0)
any grid point (0,1): (0,1)
any grid point (1,1): (1,1)
any grid point (2,1): (2,1)
any grid point (0,2): (0,2)
any grid point (1,2): (1,2)
any grid point (2,2): (2,2)
CPU time of Grid2Dit.py: 0.3 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running Grid2Dit.py _verify1_Grid2Ditv
interior points [1:2,1:2]
boundary points [2:3,1:2]
boundary points [1:2,2:3]
boundary points [0:1,1:2]
boundary points [1:2,0:1]
corners points [0:1,0:1]
corners points [2:3,0:1]
corners points [2:3,2:3]
corners points [0:1,2:3]
all points [0:3,0:3]
CPU time of Grid2Dit.py: 0.3 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running SparseVec.py 
v[1]=0 v[7]=2.2
v[200] = 0.1; index (200) out of bounds (0:99)
v[7] = (1,2); only numbers can be assigned
SparseVec(100): {1: 0.15: 2.2}
CPU time of SparseVec.py: 0.1 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running cmlparsing.py -d mydir --confirm file1 file2 f3
options= [('-d', 'mydir'), ('--confirm', '')]
args= ['file1', 'file2', 'f3']
mydir True ['file1', 'file2', 'f3']
CPU time of cmlparsing.py: 0.3 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running copytypes.py 
3
a is b: False
3
a.x=9, b_assign.x=9, b_shallow.x=-99, b_deep.x=-99
a.x=[8, 3], b_assign.x=[8, 3], b_shallow.x=[8, 3], b_deep.x=[-2, 3]
b_assign=[4, 3, 5, 999, x=-6]
b_shallow=[4, 3, 5, ['some string', 2], x=-6]
b_deep=[4, 3, 5, ['some string', 2], x=-9]
b_slice=[4, 3, 5, ['some string', 2], x=-6]

b_assign={'key2': ('str', 8, x=3), 'key1': 2}
b_shallow={'key2': ('str', 8, x=3), 'key1': -99}
b_deep={'key2': ('str', 8, x=-9), 'key1': -99}
CPU time of copytypes.py: 0.1 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running exceptions.py 
recovered from any error...
recovered from IOError or ValueError
exceptions.ValueError invalid literal for float(): s
xcoor was successfully read from file [1.0, 1.1000000000000001, 1.2, 1.2, 1.3999999999999999, 1.5]
tmp.grid does not contain numbers only
invalid literal for float(): s
CPU time of exceptions.py: 0.1 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running funcwrap_test.py 
constant: 2.0 WrapNo2Callable
string formula of x: 2.0 StringFunction
string formula of t: 2.0 StringFunction
string formula with parameters: 2.0 StringFunction
interpolate discrete data (at a grid point): 2.0
interpolate again (not at a grid point): 2.02 2.02
interpolation object type: WrapDiscreteData2Callable
user-defined function 2.0 function
user-defined anynomous lambda function: 2.0 function
user-defined class with __call__ method: 2.0 MyClass
3D string formula: 4.0 StringFunction
This is the formula: 1+2*x+3*y+4*z
Construction of instance: StringFunction('1+2*x+3*y+4*z', independent_variables=('x', 'y', 'z'), )
interpolate 3D discrete data: 4.0 WrapDiscreteData2Callable
CPU time of funcwrap_test.py: 0.3 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running integrate.py Simpson
integral of f(x)=2*x from -1 to 1 (=0): 0.0
f(0)=0
f(0.5)=1
f(1)=2
f(1)=2
f(1.5)=3
f(2)=4
integral of f(x)=2*x from 0 to 2 (=4): 4.0
CPU time of integrate.py: 0.1 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running integrate.py Trapezoidal
integral of f(x)=2*x from -1 to 1 (=0): 0.0
f(0)=0
f(1)=2
f(1)=2
f(2)=4
integral of f(x)=2*x from 0 to 2 (=4): 4.0
CPU time of integrate.py: 0.0 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running integrate.py GaussLegendre2
integral of f(x)=2*x from -1 to 1 (=0): 0.0
f(0.211325)=0.42265
f(0.788675)=1.57735
f(1.21132)=2.42265
f(1.78868)=3.57735
integral of f(x)=2*x from 0 to 2 (=4): 4.0
CPU time of integrate.py: 0.0 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running scope.py 
main: globals= {'a': 1, 'f1': <function f1 at 0xb7ced33c>, 'C': <class __main__.C at 0xb7d081ac>, 'B': <class __main__.B at 0xb7d0817c>, 'f4': <function f4 at 0xb7ced454>, 'f': <function f at 0xb7ced224>, '__builtins__': <module '__builtin__' (built-in)>, '__file__': './scope.py', 'sys': <module 'sys' (built-in)>, 'f3': <function f3 at 0xb7ced41c>, '__name__': '__main__', '__doc__': None}
main: locals= {'a': 1, 'f1': <function f1 at 0xb7ced33c>, 'C': <class __main__.C at 0xb7d081ac>, 'B': <class __main__.B at 0xb7d0817c>, 'f4': <function f4 at 0xb7ced454>, 'f': <function f at 0xb7ced224>, '__builtins__': <module '__builtin__' (built-in)>, '__file__': './scope.py', 'sys': <module 'sys' (built-in)>, 'f3': <function f3 at 0xb7ced41c>, '__name__': '__main__', '__doc__': None}
f:  locals: {'a': 2, 'x': 10} local a: 2
global a: 1
None
locals: {'a': 4, 'self': <__main__.C instance at 0xb7cf118c>}
vars(self): {'a': 3}
self.a: 3
local a: 4 global a: 1
-1 1 3
-1 1 3
3 5
Hello, in f2 inside f1
Could not assign b in f2; it made b local to f2
exceptions.UnboundLocalError local variable 'b' referenced before assignment
Error occured at line 48
Hello, in f2 inside f3
4
Hello, in f2 inside f1
b in if b block: 3
b in f2: 3
b in f4 2
CPU time of scope.py: 0.0 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running type.py 
removing directory tmp_1
removing directory tmp_0
removing directory tmp_2
removing directory tmp_3
removing directory tmp_4
removing directory tmp_5
removing directory tmp_6
removing directory tmp_7
removing directory tmp_8
removing directory tmp_9
removing file tmp__0
removing file tmp__1
removing file tmp__2
removing file tmp__3
removing file tmp__4
removing file tmp__5
removing file tmp__6
removing file tmp__7
removing file tmp__8
removing file tmp__9
text is <type 'str'> <type 'basestring'>
1.28736 is <type 'float'>
['sub', 'list'] is <type 'list'>
{'heterogeneous': 1, 'sub': 'dictionary'} is <type 'dict'>
('some', 'sub', 'tuple') is <type 'tuple'>
888 is (<type 'int'>, <type 'long'>)
<__main__.MyClass instance at 0xb7db2fec> is __main__.MyClass
text is <type 'str'>
1.28736 is <type 'float'>
['sub', 'list'] is <type 'list'>
{'heterogeneous': 1, 'sub': 'dictionary'} is <type 'dict'>
('some', 'sub', 'tuple') is <type 'tuple'>
888 is <type 'int'>
<__main__.MyClass instance at 0xb7db2fec> is <type 'instance'>
text is <type 'str'>
1.28736 is <type 'float'>
['sub', 'list'] is <type 'list'>
{'heterogeneous': 1, 'sub': 'dictionary'} is <type 'dict'>
('some', 'sub', 'tuple') is <type 'tuple'>
888 is <type 'int'>
<__main__.MyClass instance at 0xb7db2fec> is <type 'instance'>
The next call should raise an exception:
Yes, got an exception:
remove() argument 1 must be a string or a list, not a <type 'module'>
CPU time of type.py: 0.1 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running typedlist.py 
detected wrong type; items must be int, not float
detected wrong type; items must be A, not B
[1, 4, 3, 2, 9, 2, 3, 9, 2, 3]
detected wrong type; items must be int, not float
detected wrong type; items must be int, not A
CPU time of typedlist.py: 0.1 seconds on ubuntu i686, Linux


#### Test: ./tests.verify running xdr.py 
3.2 5 [1.0, 0.10000000000000001, 0.001]
CPU time of xdr.py: 0.0 seconds on ubuntu i686, Linux

