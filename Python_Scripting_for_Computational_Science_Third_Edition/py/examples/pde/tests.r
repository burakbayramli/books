./tests.verify: test performed on 2004.03.31 


#### Test: ./tests.verify running wave1D_func1.py test_solver1  4 "'scalar'" 
test_solver1(4, 'scalar')
CPU time: 0.04
Max value in final u:  8.0902e-01
CPU time of wave1D_func1.py: 0.3 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running wave1D_func1.py test_solver1  4 "'vectorized'" 
test_solver1(4, 'vectorized')
CPU time: 0.01
Max value in final u:  8.0902e-01
CPU time of wave1D_func1.py: 0.2 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running wave1D_func1.py test_solver1c 4 "'vectorized'" 
test_solver1c(4, 'vectorized')
CPU time: 0.01
Max value in final u:  8.0902e-01
CPU time of wave1D_func1.py: 0.3 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running wave1D_func1.py test_solver2  4 0
test_solver2(4, 0)
CPU time: 0.0
[array([   0.0000e+00,    8.6603e-01,    8.6603e-01,    0.0000e+00,
             -8.6603e-01,  -8.6603e-01,   0.0000e+00]), array([   0.0000e+00,  -4.3301e-01,  -4.3301e-01,    0.0000e+00,
               4.3301e-01,    4.3301e-01,    0.0000e+00]), array([   0.0000e+00,  -4.3301e-01,  -4.3301e-01,    0.0000e+00,
               4.3301e-01,    4.3301e-01,    0.0000e+00]), array([   0.0000e+00,    8.6603e-01,    8.6603e-01,    0.0000e+00,
             -8.6603e-01,  -8.6603e-01,    0.0000e+00]), array([   0.0000e+00,  -4.3301e-01,  -4.3301e-01,   0.0000e+00,
               4.3301e-01,    4.3301e-01,    0.0000e+00]), array([   0.0000e+00,  -4.3301e-01,  -4.3301e-01,    0.0000e+00,
               4.3301e-01,    4.3301e-01,    0.0000e+00]), array([   0.0000e+00,    8.6603e-01,    8.6603e-01,    0.0000e+00,
             -8.6603e-01,  -8.6603e-01,    0.0000e+00])]
CPU time of wave1D_func1.py: 0.2 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running wave1D_func1.py test_solver3 "'vectorized'" 
test_solver3('vectorized')
Max error:  0.0000e+00
CPU time of wave1D_func1.py: 0.3 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running wave1D_func1.py test_solver_plug 0 "'vectorized'" 1500
test_solver_plug(0, 'vectorized', 1500)
CPU time: vectorized version = 1.37
error in computations
CPU time of wave1D_func1.py: 1.6 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running wave1D_func2.py test_radial_waves 0 '"vectorized"' 
test_radial_waves(0, "vectorized")
CPU time vectorized version: 1.0
v[0] final time: -7.6151e-02
CPU time of wave1D_func2.py: 1.2 seconds on hplx30 i686, Linux


#### Test: ./tests.verify running wave1D_class.py test_WaveEq1_plug 0 '"vectorized"' 
test_WaveEq1_plug(0, "vectorized")
registered parameters:

dt
n
safety_factor
scheme_coding
tstop
user_action
bc_0
bc_L
c
f
I
L
registered parameters:

dt
n
safety_factor
scheme_coding
tstop
user_action
bc_0
bc_L
c
f
I
L
c=1 is assigned
bc_0=0 is assigned
f=0 is assigned
I=<function I at 0x410a379c> is assigned
user_action is not registered in
{'c': 1, 'bc_0': 0, 'f': 0, 'I': <function I at 0x410a379c>, 'L': 1, 'bc_L': 0}
user_action=<bound method SolverWithViz.action of <__main__.SolverWithViz instance at 0x410a148c>> is assigned
n is not registered in
{'c': 1, 'bc_0': 0, 'f': 0, 'I': <function I at 0x410a379c>, 'L': 1, 'bc_L': 0}
n=50 is assigned
bc_L=0 is assigned
tstop is not registered in
{'c': 1, 'bc_0': 0, 'f': 0, 'I': <function I at 0x410a379c>, 'L': 1, 'bc_L': 0}
tstop=9.95 is assigned
scheme_coding is not registered in
{'c': 1, 'bc_0': 0, 'f': 0, 'I': <function I at 0x410a379c>, 'L': 1, 'bc_L': 0}
scheme_coding=vectorized is assigned
f=<function <lambda> at 0x410a37d4> is assigned
bc_0 = <py4cs.numpytools.WrapNo2Callable instance at 0x410a15ec>
bc_L = <py4cs.numpytools.WrapNo2Callable instance at 0x410a18cc>
c = <py4cs.numpytools.WrapNo2Callable instance at 0x410a186c>
f = <function <lambda> at 0x410a37d4>
I = <function I at 0x410a379c>
L = 1
dt = 0.02
n = 50
safety_factor = 1.0
scheme_coding = vectorized
tstop = 9.95
user_action = <bound method SolverWithViz.action of <__main__.SolverWithViz instance at 0x410a148c>>
CPU time: 0.09
error in computations
CPU time of wave1D_class.py: 0.4 seconds on hplx30 i686, Linux

