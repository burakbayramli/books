tests.verify: test performed on 2006.05.21 


#### Test: tests.verify running hw.py 1.2
Hello, World! sin(1.2)=0.932039085967
CPU time of hw.py: 0.1 seconds on hplx30 i686, Linux


#### Test: tests.verify running datatrans1.py .datatrans_infile tmp1file
CPU time of datatrans1.py: 0.1 seconds on hplx30 i686, Linux



----- appending file tmp1file ------
0.1   5.36092e-01
0.2   3.12343e+00
0.3   5.87269e+00
0.4   3.12343e+00

#### Test: tests.verify running datatrans2.py .datatrans_infile tmp2file
CPU time of datatrans2.py: 0.1 seconds on hplx30 i686, Linux



----- appending file tmp2file ------
0.1   5.36092e-01
0.2   3.12343e+00
0.3   5.87269e+00
0.4   3.12343e+00

#### Test: tests.verify running datatrans3a.py .datatrans_infile tmp3afile
CPU time of datatrans3a.py: 0.4 seconds on hplx30 i686, Linux



----- appending file tmp3afile ------
0.1	0.536092
0.2	3.12343
0.3	5.87269
0.4	3.12343

#### Test: tests.verify running datatrans3b.py .datatrans_infile tmp3bfile
CPU time of datatrans3b.py: 0.4 seconds on hplx30 i686, Linux



----- appending file tmp3bfile ------
0.1 0.536092 
0.2 3.12343 
0.3 5.87269 
0.4 3.12343 

#### Test: tests.verify running convert1.py .convert_infile1
CPU time of convert1.py: 0.1 seconds on hplx30 i686, Linux



----- appending file tmp-measurements.dat ------
           0  0.00000e+00
         1.5  1.00000e-01
           3  2.00000e-01


----- appending file tmp-model1.dat ------
           0  1.00000e-01
         1.5  1.00000e-01
           3  2.00000e-01


----- appending file tmp-model2.dat ------
           0  1.00000e+00
         1.5  1.88000e-01
           3  2.50000e-01

#### Test: tests.verify running convert2.py .convert_infile1
y dictionary:
{'tmp-model2': [1.0, 0.188, 0.25], 'tmp-model1': [0.10000000000000001, 0.10000000000000001, 0.20000000000000001], 'tmp-measurements': [0.0, 0.10000000000000001, 0.20000000000000001]}
CPU time of convert2.py: 0.1 seconds on hplx30 i686, Linux



----- appending file tmp-measurements.dat ------
           0  0.00000e+00
         1.5  1.00000e-01
           3  2.00000e-01


----- appending file tmp-model1.dat ------
           0  1.00000e-01
         1.5  1.00000e-01
           3  2.00000e-01


----- appending file tmp-model2.dat ------
           0  1.00000e+00
         1.5  1.88000e-01
           3  2.50000e-01

#### Test: tests.verify running simviz1.py -A 5.0 -tstop 2 -case tmp4
CPU time of simviz1.py: 0.2 seconds on hplx30 i686, Linux



----- appending file tmp4/tmp4.i ------

        1
        0.7
        5
        y
        5
        6.28319
        0.2
        2
        0.05
        

----- appending file tmp4/tmp4.gnuplot ------

set title 'tmp4: m=1 b=0.7 c=5 f(y)=y A=5 w=6.28319 y0=0.2 dt=0.05';
plot 'sim.dat' title 'y(t)' with lines;

set size ratio 0.3 1.5, 1.0;  
# define the postscript output format:
set term postscript eps monochrome dashed 'Times-Roman' 28;
# output file containing the plot:
set output 'tmp4.ps';
# basic plot command:
plot 'sim.dat' title 'y(t)' with lines;
# make a plot in PNG format:
set term png small;
set output 'tmp4.png';
plot 'sim.dat' title 'y(t)' with lines;


----- appending file tmp4/tmp4.ps (just 30 lines) ------
%!PS-Adobe-2.0 EPSF-2.0
%%Title: tmp4.ps
%%Creator: gnuplot 4.0 patchlevel 0
%%CreationDate: Sun May 21 12:04:22 2006
%%DocumentFonts: (atend)
%%BoundingBox: 50 50 590 302
%%Orientation: Portrait
%%EndComments
/gnudict 256 dict def
gnudict begin
/Color false def
/Solid false def
/gnulinewidth 5.000 def
/userlinewidth gnulinewidth def
/vshift -93 def
/dl {10.0 mul} def
/hpt_ 31.5 def
/vpt_ 31.5 def
/hpt hpt_ def
/vpt vpt_ def
/Rounded false def
/M {moveto} bind def
/L {lineto} bind def
/R {rmoveto} bind def
/V {rlineto} bind def
/N {newpath moveto} bind def
/C {setrgbcolor} bind def
/f {rlineto fill} bind def
/vpt2 vpt 2 mul def
/hpt2 hpt 2 mul def

#### Test: tests.verify running loop4simviz2.py c 5 30 10 -yaxis -0.7 0.7 -A 5.0 -tstop 2 -case tmp4 -noscreenplot
MPEG ENCODER STATS (1.5b)
------------------------
TIME STARTED:  Sun May 21 12:04:31 2006
MACHINE:  unknown
FIRST FILE:  ./tmp_0000.ppm
LAST FILE:  ./tmp_0002.ppm
PATTERN:  i
GOP_SIZE:  30
SLICES PER FRAME:  1
RANGE:  +/-10
PIXEL SEARCH:  HALF
PSEARCH:  LOGARITHMIC
BSEARCH:  CROSS2
QSCALE:  8 10 25
REFERENCE FRAME:  ORIGINAL


Creating new GOP (closed = T) before frame 0
FRAME 0 (I):  0 seconds  (4654800 bits/s output)
FRAME 1 (I):  0 seconds  (4792800 bits/s output)
FRAME 2 (I):  0 seconds  (4861920 bits/s output)


TIME COMPLETED:  Sun May 21 12:04:32 2006
Total time:  1 seconds

-------------------------
*****I FRAME SUMMARY*****
-------------------------
  Blocks:     5772     (476670 bits)     (   82 bpb)
  Frames:        3     (476984 bits)     (158994 bpf)     (99.8% of total)
  Compression:   74:1     (   0.3228 bpp)
  Seconds:          0     (   4.8649 fps)  (  2396160 pps)  (     9360 mps)
---------------------------------------------
Total Compression:   74:1     (   0.3234 bpp)
Total Frames Per Second:  3.000000 (5772 mps)
CPU Time:  4.864865 fps     (9359 mps)
Total Output Bit Rate (30 fps):  4778800 bits/sec
MPEG file created in :  movie.mpeg


======FRAMES READ:  3
gs -q -dBATCH -dNOPAUSE -sDEVICE=ppm  -sOutputFile=tmp_0000.ppm tmp_c_5/tmp_c_5.ps
tmp_c_5/tmp_c_5.ps transformed via gs to tmp_0000.ppm (1503 Kb)
gs -q -dBATCH -dNOPAUSE -sDEVICE=ppm  -sOutputFile=tmp_0001.ppm tmp_c_15/tmp_c_15.ps
tmp_c_15/tmp_c_15.ps transformed via gs to tmp_0001.ppm (1503 Kb)
gs -q -dBATCH -dNOPAUSE -sDEVICE=ppm  -sOutputFile=tmp_0002.ppm tmp_c_25/tmp_c_25.ps
tmp_c_25/tmp_c_25.ps transformed via gs to tmp_0002.ppm (1503 Kb)
mpeg movie in output file movie.mpeg
running python simviz2.py -yaxis -0.7 0.7 -A 5.0 -tstop 2 -case tmp4 -noscreenplot -c 5 -case tmp_c_5
running python simviz2.py -yaxis -0.7 0.7 -A 5.0 -tstop 2 -case tmp4 -noscreenplot -c 15 -case tmp_c_15
running python simviz2.py -yaxis -0.7 0.7 -A 5.0 -tstop 2 -case tmp4 -noscreenplot -c 25 -case tmp_c_25
converting PNG files to animated GIF:
convert -delay 50 -loop 1000 tmp_c_5/tmp_c_5.png tmp_c_15/tmp_c_15.png tmp_c_25/tmp_c_25.png tmp_c.gif
converting PostScript files to an MPEG movie:
ps2mpeg.py tmp_c_5/tmp_c_5.ps tmp_c_15/tmp_c_15.ps tmp_c_25/tmp_c_25.ps
epsmerge -o tmp_c_runs.ps -x 2 -y 3 -par tmp_c_5/tmp_c_5.ps tmp_c_15/tmp_c_15.ps tmp_c_25/tmp_c_25.ps
CPU time of loop4simviz2.py: 8.3 seconds on hplx30 i686, Linux



----- appending file tmp_c_runs.html ------
<HTML><BODY BGCOLOR="white">
<H1>c=5</H1> <IMG SRC="tmp_c_5/tmp_c_5.png">
<H1>c=15</H1> <IMG SRC="tmp_c_15/tmp_c_15.png">
<H1>c=25</H1> <IMG SRC="tmp_c_25/tmp_c_25.png">
<H1>Movie</H1> <IMG SRC="tmp_c.gif">
<H1><A HREF="tmp_c.mpeg">MPEG Movie</A></H1>
</BODY></HTML>

#### Test: tests.verify running NumPy_basics.py 
zeros(n, Float): <type 'array'> d [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.]
zeros(n) <type 'array'> l [0 0 0 0 0 0 0 0 0 0]
arrayrange(-5, 5, 0.5) <type 'array'> d [-5.  -4.5 -4.  -3.5 -3.  -2.5 -2.  -1.5 -1.  -0.5  0.   0.5  1.   1.5  2. 
       2.5  3.   3.5  4.   4.5  5. ]
y = sin(x/2.0)*3.0: <type 'array'> d [-1.79541643 -2.33421959 -2.72789228 -2.95195784 -2.99248496 -2.84695386
      -2.52441295 -2.04491628 -1.43827662 -0.74221188  0.          0.74221188
       1.43827662  2.04491628  2.52441295  2.84695386  2.99248496  2.95195784
       2.72789228  2.33421959  1.79541643]
pl = [0, 1.2, 4, -9.1, 5, 8]; array(pl, typecode=Float) <type 'array'> [ 0.   1.2  4.  -9.1  5.   8. ]

creating arrays of length 1E+06 ... 
fromfunction took 0.66 s and arange&sin took 0.39 s for length 1000000
1.2
[  0.  10.]
[[  0.   1.   2.   3.   4.   5.]
 [  6.   7.   8.   9.  10.  11.]
 [ 12.  13.  14.  15.  16.  17.]
 [ 18.  19.  20.  21.  22.  23.]
 [ 24.  25.  26.  27.  28.  29.]]
[[  6.   8.  10.]
 [ 12.  14.  16.]]
[[  2.   4.]
 [ 20.  22.]]
[[  2.   4.]
 [ 20.  22.]]
a[0,0]=2  a[0,1]=6  a[0,2]=12 
a[1,0]=4  a[1,1]=12  a[1,2]=24 
a.shape = (2,3); a= [[  2.   6.  12.]
 [  4.  12.  24.]]
a.shape = (size(a),); a= [  2.   6.  12.   4.  12.  24.]
b = 3*a - 1; b = clip(b, 0.1, 1.0E+20); c = cos(b) [  5.  17.  35.  11.  35.  71.] [ 0.28366219 -0.27516334 -0.90369221  0.0044257  -0.90369221 -0.30902273]
in-place operations:
multiply(a, 3.0, a); a= [  6.  18.  36.  12.  36.  72.]
subtract(a, 1.0, a); a= [  5.  17.  35.  11.  35.  71.]
divide  (a, 3.0, a); a= [  1.66666667   5.66666667  11.66666667   3.66666667  11.66666667  23.66666667]
add     (a, 1.0, a); a= [  2.66666667   6.66666667  12.66666667   4.66666667  12.66666667  24.66666667]
power   (a, 2.0, a); a= [   7.11111111   44.44444444  160.44444444   21.77777778  160.44444444
       608.44444444]
a *= 3.0; a= [   21.33333333   133.33333333   481.33333333    65.33333333   481.33333333
       1825.33333333]
a -= 1.0; a= [   20.33333333   132.33333333   480.33333333    64.33333333   480.33333333
       1824.33333333]
a /= 3.0; a= [   6.77777778   44.11111111  160.11111111   21.44444444  160.11111111
       608.11111111]
a += 1.0; a= [   7.77777778   45.11111111  161.11111111   22.44444444  161.11111111
       609.11111111]
a **= 2.0; a= [  6.04938272e+01   2.03501235e+03   2.59567901e+04   5.03753086e+02
        2.59567901e+04   3.71016346e+05]
a[2:4] = -1; a[-1] = a[0]; a= [[ 0.   1.2  4. ]
 [ 0.   1.2  4. ]]
a.shape = (3,2); a[:,0] [ 0.   4.   1.2]
a[:,1::2] [[ 1.2]
 [ 0. ]
 [ 4. ]]
transpose(a):  [[ 0.   4.   1.2]
 [ 1.2  0.   4. ]]
b array entries are of type Float so no casting is necessary
arrayrange(-5, 5, 0.5); variable type=<type 'array'> typecode=d
arrayrange(-5, 5, 1); variable type=<type 'array'> typecode=l
arrayrange(-5.0, 5, 1); variable type=<type 'array'> typecode=d
arrayrange(-5, 5, 0.5, Complex); variable type=<type 'array'> typecode=D
real part of x: [-5.  -4.5 -4.  -3.5 -3.  -2.5 -2.  -1.5 -1.  -0.5  0.   0.5  1.   1.5  2. 
       2.5  3.   3.5  4.   4.5]
imaginary part of x: [ 0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.  0.
       0.]
eval(file.read()) works
read from binary (pickled) file: b1= [[  1.   2.]
 [ 11.  12.]] b2= [[  4.   5.]
 [ 14.  15.]]
read from binary file: b= [[  1.   2.   3.   4.   5.   6.   7.   8.   9.  10.]
 [ 11.  12.  13.  14.  15.  16.  17.  18.  19.  20.]]


--------- random numbers -------------

random number on (0,1): 0.723068520556
unform random number on (-1,1): -0.874106242503
N(0,1) uniform random number: 1.42433423327
mean of 10000 random uniform random numbers:
on (0,1): 0.49683461788 (should be 0.5)
on (-1,1): -0.00210852790364 (should be 0)
generated 10000 N(0,1) samples with
mean 0.00870529 and st.dev. 0.993361 using RandomArray.normal
probability N(0,1) < 1.5: 0.93


--------- linear algebra -------------

correct solution
correct solution
LinearAlgebra.determinant(A) = 1.19047619048e-05
B is the inverse of A, check ||A*B-I|| = 0, result: 7.46661081945e-13
det(A)=1.19048e-05
eigenvalue 10.6047 has corresponding vector
[ 0.47326857  0.49642858  0.51003386  0.51907706]
eigenvalue 0.215261 has corresponding vector
[ 0.7065385   0.08012043 -0.33075785 -0.62047023]
eigenvalue 0.017824 has corresponding vector
[ 0.06174023 -0.67059248 -0.07005871  0.73592503]
eigenvalue 0.000292584 has corresponding vector
[-0.04451401  0.33097206 -0.79128654  0.51219294]
A: [[ 3.          2.5         2.33333333  2.25      ]
 [ 3.          2.66666667  2.5         2.4       ]
 [ 3.          2.75        2.6         2.5       ]
 [ 3.          2.8         2.66666667  2.57142857]]
x: [ 0.   0.5  1.   1.5]
b: [ 6.95833333  7.43333333  7.725       7.92380952]
A*x = [[ 0.          1.25        2.33333333  3.375     ]
 [ 0.          1.33333333  2.5         3.6       ]
 [ 0.          1.375       2.6         3.75      ]
 [ 0.          1.4         2.66666667  3.85714286]]
b*x = [  0.           3.71666667   7.725       11.88571429]
matrixmultiply(A,x) = [ 6.95833333  7.43333333  7.725       7.92380952]
dot(A,x) = [ 6.95833333  7.43333333  7.725       7.92380952]
innerproduct(b,x) = 23.3273809524
dot(b,x) = 23.3273809524
Matrix M: Matrix([[ 3.        ,  2.5       ,  2.33333333,  2.25      ],
       [ 3.        ,  2.66666667,  2.5       ,  2.4       ],
       [ 3.        ,  2.75      ,  2.6       ,  2.5       ],
       [ 3.        ,  2.8       ,  2.66666667,  2.57142857]])
column vector xm: [[ 0. ]
 [ 0.5]
 [ 1. ]
 [ 1.5]]
M*xm = Matrix([[ 6.95833333],
       [ 7.43333333],
       [ 7.725     ],
       [ 7.92380952]])
row vector bm: Matrix([       [ 6.95833333,  7.43333333,  7.725     ,  7.92380952]])
bm*xm = Matrix([       [ 23.32738095]]) (inner product)
xm*xb = Matrix([[  0.        ,   0.        ,   0.        ,   0.        ],
       [  3.47916667,   3.71666667,   3.8625    ,   3.96190476],
       [  6.95833333,   7.43333333,   7.725     ,   7.92380952],
       [ 10.4375    ,  11.15      ,  11.5875    ,  11.88571429]]) (outer product)

performing some timings of "somefunc*" implementations...
 somefunc_NumPy (1 calls): elapsed=8.97336, CPU=8.09
 somefunc_NumPy2 (1 calls): elapsed=0.0820889, CPU=0.09
 somefunc_NumPy3 (1 calls): elapsed=0.118568, CPU=0.1
end of ./NumPy_basics.py
CPU time of NumPy_basics.py: 9.9 seconds on hplx30 i686, Linux


#### Test: tests.verify running leastsquares.py 
CPU time of leastsquares.py: 0.6 seconds on hplx30 i686, Linux


#### Test: tests.verify running SciPy.py vectorization
control numbers (vectorize)  : 0.0 0.909307830212
control numbers (vectorize)  : 0.0 0.909307830212
control numbers (vectorize)  : 0.0 0.909307830212
control numbers (hand-coded) : 0.0 0.909307830212
CPU time of somefuncv: 6.5
CPU time of somefuncv2: 1.47
CPU time of somefuncv3: 6.61
CPU time of hand-coded function: 0.15
CPU time: vectorize(NumPy sin)/hand-coded = 43.3
CPU time: vectorize(math.sin)/hand-coded = 9.8
CPU time of SciPy.py: 15.4 seconds on hplx30 i686, Linux


#### Test: tests.verify running SciPy.py Oscillator 0.05
CPU time of odeint: 0.18
CPU time of oscillator: 0.18
CPU time of SciPy.py: 1.3 seconds on hplx30 i686, Linux


#### Test: tests.verify running SciPy.py integrate
2.0 2.22044604925e-14
2.0 2.22044604925e-14
CPU time of SciPy.py: 0.6 seconds on hplx30 i686, Linux


#### Test: tests.verify running ScientificPython.py 
testing numbers with physical units:
arithmetics with PhysicalQuantity instances: 10.56 kg*km/s**2
converted to basic SI units: 10560.0 m*kg/s**2
converted to Mega Newton: 0.01056 MN
adding 0.1 kPa m^2: 0.01066 MN
str(F): 0.01066 MN
extract float value: 0.01066
10660.0 m*kg/s**2
0 Celcius in Farenheit: 32.0 degF


Testing automatic derivatives:
(function value, [d/dx, d/dy, d/dz]):
(6.0250000000000004, [3.0, -1.0, 1.0])
(sin(0), [cos(0)]): (0.0, [1.0])
3rd order derivatives of x^3: (1, [3], [[6]], [[[6]]])
(40, [3, -1, 20], [[0, 0, 0], [0, 0, 0], [0, 0, 20]])
d^2(somefunc)/dzdx: 0
d^2(somefunc)/dz^2: 20


testing interpolation:
interpolated: -0.942369478495  exact: -0.943548668636
interpolated derivative: 0.331095923354  exact: 0.331233920237
definite integral: 1.83753871398  exact: 1.83907152908
interpolation in 2D grid: 0.946401714384  exact value: 0.968105223808


testing nonlinear least squares:
([1.0864630262152011, 2.0402214672667118, 1.9767714371137151, 0.99937257343868868], 8.2409274338033922e-06)
guess: [ 1.  2.  2.  1.]
     deviation: [-0.0865 -0.0402  0.0232  0.0006]
guess: [ 0.95  1.95  1.95  0.95]
     deviation: [-0.171   0.1275  0.0785  0.0119]
guess: [ 0.9  1.9  1.9  0.9]
     deviation: [-0.1974  0.3622  0.1669  0.0193]
guess: [ 0.85  1.85  1.85  0.85]
     deviation: [-0.3122  0.4309  0.2334  0.0284]
guess: [ 1.2  2.2  2.2  1.2]
     deviation: [-0.9721 -0.6498 -0.0425 -0.0059]
guess: [ 0.75  1.75  1.75  0.75]
     deviation: [-0.6266  0.5119  0.3973  0.0569]


Testing statistical computations:
mean=1.00  standard deviation=0.50  skewness=-0.0 median=1.00, kurtosis=3.01
histogram array:
[[ -1.2254e+00   1.1255e-04]
 [ -1.1365e+00   2.2511e-04]
 [ -1.0477e+00   6.7533e-04]
 [ -9.5885e-01   4.5022e-04]
 [ -8.7001e-01   2.2511e-04]
 [ -7.8116e-01   1.2381e-03]
 [ -6.9231e-01   2.3636e-03]
 [ -6.0347e-01   5.5152e-03]
 [ -5.1462e-01   8.6667e-03]
 [ -4.2578e-01   1.3619e-02]
 [ -3.3693e-01   2.5212e-02]
 [ -2.4809e-01   3.6468e-02]
 [ -1.5924e-01   5.2563e-02]
 [ -7.0394e-02   8.1039e-02]
 [  1.8452e-02   1.1818e-01]
 [  1.0730e-01   1.5679e-01]
 [  1.9614e-01   2.1712e-01]
 [  2.8499e-01   2.8206e-01]
 [  3.7383e-01   3.7075e-01]
 [  4.6268e-01   4.6429e-01]
 [  5.5153e-01   5.3497e-01]
 [  6.4037e-01   6.2389e-01]
 [  7.2922e-01   6.8411e-01]
 [  8.1806e-01   7.2800e-01]
 [  9.0691e-01   7.7516e-01]
 [  9.9575e-01   7.9452e-01]
 [  1.0846e+00   8.0465e-01]
 [  1.1734e+00   7.4162e-01]
 [  1.2623e+00   6.9075e-01]
 [  1.3511e+00   6.2344e-01]
 [  1.4400e+00   5.4533e-01]
 [  1.5288e+00   4.5866e-01]
 [  1.6177e+00   3.8629e-01]
 [  1.7065e+00   2.8836e-01]
 [  1.7954e+00   2.2061e-01]
 [  1.8842e+00   1.6467e-01]
 [  1.9731e+00   1.2167e-01]
 [  2.0619e+00   8.4754e-02]
 [  2.1507e+00   5.4476e-02]
 [  2.2396e+00   3.5117e-02]
 [  2.3284e+00   2.4312e-02]
 [  2.4173e+00   1.3507e-02]
 [  2.5061e+00   8.4416e-03]
 [  2.5950e+00   5.2901e-03]
 [  2.6838e+00   2.1385e-03]
 [  2.7727e+00   1.3507e-03]
 [  2.8615e+00   9.0044e-04]
 [  2.9504e+00   1.1255e-04]
 [  3.0392e+00   4.5022e-04]
 [  3.1281e+00   3.3766e-04]]
50 [-1.2254 -1.1365 -1.0477 -0.9589 -0.87   -0.7812 -0.6923 -0.6035 -0.5146
      -0.4258 -0.3369 -0.2481 -0.1592 -0.0704  0.0185  0.1073  0.1961  0.285 
       0.3738  0.4627  0.5515  0.6404  0.7292  0.8181  0.9069  0.9958  1.0846
       1.1734  1.2623  1.3511  1.44    1.5288  1.6177  1.7065  1.7954  1.8842
       1.9731  2.0619  2.1507  2.2396  2.3284  2.4173  2.5061  2.595   2.6838
       2.7727  2.8615  2.9504  3.0392  3.1281]
50 [  1.1255e-04   2.2511e-04   6.7533e-04   4.5022e-04   2.2511e-04   1.2381e-03
        2.3636e-03   5.5152e-03   8.6667e-03   1.3619e-02   2.5212e-02
        3.6468e-02   5.2563e-02   8.1039e-02   1.1818e-01   1.5679e-01
        2.1712e-01   2.8206e-01   3.7075e-01   4.6429e-01   5.3497e-01
        6.2389e-01   6.8411e-01   7.2800e-01   7.7516e-01   7.9452e-01
        8.0465e-01   7.4162e-01   6.9075e-01   6.2344e-01   5.4533e-01
        4.5866e-01   3.8629e-01   2.8836e-01   2.2061e-01   1.6467e-01
        1.2167e-01   8.4754e-02   5.4476e-02   3.5117e-02   2.4312e-02
        1.3507e-02   8.4416e-03   5.2901e-03   2.1385e-03   1.3507e-03
        9.0044e-04   1.1255e-04   4.5022e-04   3.3766e-04]
CPU time of ScientificPython.py: 2.0 seconds on hplx30 i686, Linux


#### Test: tests.verify running commontasks.py 
Hello Pipe World
./commontasks.py : cannot read file 'qqq'
An exception of type
  exceptions.IOError 
occurred, with value
   [Errno 2] No such file or directory: 'qqq'
arglist= ['myarg1', 'displacement', 'tmp.ps']
filename= myarg1  plottitle= displacement  psfile= tmp.ps
arglist= ['myarg1', 'displacement', 'tmp.ps', 'myvar2']
entry is  myarg1
entry is  displacement
entry is  tmp.ps
entry is  myvar2
In-place manipulation of array entries:
A[0]=1.2
A[1]=-3.4
A[2]=5.5
A[3]=-9
A[4]=100
No negative numbers:
A[0]=1.2
A[1]=0
A[2]=5.5
A[3]=0
A[4]=100
a 'foreach'-type loop does not work:
A[0]=1.2
A[1]=-3.4
A[2]=5.5
A[3]=-9
A[4]=100

split with re.split:
words1[0] = "iteration"
words1[1] = "12:"
words1[2] = "eps="
words1[3] = "1.245E-05"

split with string.split instead:
words1[0] = "iteration"
words1[2] = "12:"
words1[4] = "eps="
words1[6] = "1.245E-05"
newline1 is now [ iteration#12:#eps=#1.245E-05 ]
words2[0]=.myc_12
words2[1]=displacement
words2[2]=u(x,3.1415)
words2[3]=  no upwinding
['white', 'space', 'of', 'varying', 'length']
['white', 'space', '', '', 'of', 'varying', '', '', '', 'length']
Yes, matched regex= <_sre.SRE_Match object at 0xb7b69138>
regex sub:
#!/usr/bin/env python
import sys, math       # load system and math module
r = float(sys.argv[1]) # extract the 1st command-line argument
s = math.sin(r)
print "Hello, World! sin(" + str(r) + ")=" + str(s)



Testing dictionaries:

len(myargs)= 6
The option --myopt is not registered
The option -9.9 is not registered
dictionary: cmlargs, key= -tstop  value= 6.1
dictionary: cmlargs, key= -c_in_H  value= 9.8
cmlargs['-c_in_H']=9.8
cmlargs['-tstop']=6.1




Python lacks auto convert of strings-numbers
a =  0.6
hw.py is a plain file
stat:  33261
stat:  205289
stat:  775
stat:  1
stat:  8029
stat:  18029
stat:  206
stat:  1148205909
stat:  1148199265
stat:  1148199265
hw.py is a regular file with 206 bytes and last accessed 1148205909
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/entries
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/.test1.c.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/fdmgrid.py.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/htmlsubst.py.svn-base
0.01Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/intervalre.py.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/introre.py.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/realre.py.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/swap1.py.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/swap2.py.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/swap3.py.svn-base
0.01Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/tests.r.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/tests.verify.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/verify_log.htm.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/verify_log_details.htm.svn-base
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/.test1.c
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/fdmgrid.py
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/htmlsubst.py
0.01Mb /home/hpl/work/scripting/trunk/src/py/regex/intervalre.py
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/introre.py
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/realre.py
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/swap1.py
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/swap2.py
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/swap3.py
0.01Mb /home/hpl/work/scripting/trunk/src/py/regex/tests.r
0.01Mb /home/hpl/work/scripting/trunk/src/py/regex/tests.v
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/tests.verify
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/verify_log.htm
0.00Mb /home/hpl/work/scripting/trunk/src/py/regex/verify_log_details.htm


version 3 of tree traversal: root= /home/hpl/work/scripting/trunk/src/py/regex
/home/hpl/work/scripting/trunk/src/py/regex/.test1.c is 0.000672 Mb
/home/hpl/work/scripting/trunk/src/py/regex/realre.py is 0.001521 Mb
/home/hpl/work/scripting/trunk/src/py/regex/verify_log.htm is 0.000322 Mb
/home/hpl/work/scripting/trunk/src/py/regex/swap1.py is 0.000448 Mb
/home/hpl/work/scripting/trunk/src/py/regex/swap2.py is 0.00101 Mb
/home/hpl/work/scripting/trunk/src/py/regex/swap3.py is 0.001352 Mb
/home/hpl/work/scripting/trunk/src/py/regex/intervalre.py is 0.005451 Mb
/home/hpl/work/scripting/trunk/src/py/regex/verify_log_details.htm is 0.001306 Mb
/home/hpl/work/scripting/trunk/src/py/regex/tests.r is 0.011983 Mb
/home/hpl/work/scripting/trunk/src/py/regex/tests.v is 0.011983 Mb
/home/hpl/work/scripting/trunk/src/py/regex/tests.verify is 0.000366 Mb
/home/hpl/work/scripting/trunk/src/py/regex/htmlsubst.py is 0.000649 Mb
/home/hpl/work/scripting/trunk/src/py/regex/fdmgrid.py is 0.002941 Mb
/home/hpl/work/scripting/trunk/src/py/regex/introre.py is 0.001889 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/entries is 0.003859 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/.test1.c.svn-base is 0.000672 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/swap1.py.svn-base is 0.000448 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/intervalre.py.svn-base is 0.005451 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/swap2.py.svn-base is 0.00101 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/introre.py.svn-base is 0.001889 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/verify_log.htm.svn-base is 0.000322 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/swap3.py.svn-base is 0.001352 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/realre.py.svn-base is 0.001521 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/verify_log_details.htm.svn-base is 0.001306 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/fdmgrid.py.svn-base is 0.002941 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/tests.verify.svn-base is 0.000366 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/htmlsubst.py.svn-base is 0.000649 Mb
/home/hpl/work/scripting/trunk/src/py/regex/.svn/text-base/tests.r.svn-base is 0.011983 Mb
creating mynewdir
removing mynewdir
leading text ['listitem1', 'listitem2']
another leading text ['listitem1', 'listitem2']
No leading text ['listitem1', 'listitem2']
No leading text /home/hpl
statistics: avg=$avg, min=$min, max=$max

before swap: v1= 1.3  v2= some text
after  swap: v1= 1.3  v2= some text
files of *.ps *.gif and *.py type:
tmp_c_runs.ps
tmp1.ps
tmp2.ps
tmp.ps
tmp_c.gif
NumPy_basics.py
hw.py
leastsquares.py
datatrans1.py
datatrans2.py
datatrans3.py
ScientificPython.py
datatrans-eff.py
datatrans3a.py
datatrans3b.py
datatrans3c.py
datatrans3d.py
loop4simviz1.py
loop4simviz2.py
datatrans3_err.py
commontasks.py
simviz1.py
simviz2.py
convert1.py
convert2.py
convert3.py
SciPy.py
Output of the command perl -pe '' hw.py was

#!/usr/bin/env python
import sys, math       # load system and math module
r = float(sys.argv[1]) # extract the 1st command-line argument
s = math.sin(r)
print "Hello, World! sin(" + str(r) + ")=" + str(s)
file= /usr/home/hpl/scripting/perl/intro/hw.pl
head= /usr/home/hpl/scripting/perl/intro
tail= hw.pl
dirname = /usr/home/hpl/scripting/perl/intro
basename= hw.pl

Yes! created tmp/some/tmp/tmp


Programming with classes:
MyBase: i= 5 j= 7
MySub: i= 7 j= 8 k= 9
i1 is MyBase
i2 is MySub
i2 is MyBase too
i1.__dict__: {'i': 5, 'j': 7}
i2.__dict__: {'i': 7, 'k': 9, 'j': 8}
Names: MyBase MyBase.write
dir(i1): ['__doc__', '__init__', '__module__', 'i', 'j', 'write']
dir(i2): ['__doc__', '__init__', '__module__', 'i', 'j', 'k', 'write']
some string
['__doc__', '__init__', '__module__', 'i', 'j', 'k', 'q', 'write']





multiple inheritance:
A.set
B.set
C.set
{'a': 2, 'c': 0, 'b': 3}
list[6] raises an exception
Exception type= exceptions.IndexError
Exception value= list index out of range
average= 2.0
average= Empty argument list in average
Exception type= Empty argument list in average
Exception value= None
Exception type= exceptions.IOError
Exception value= [Errno 2] No such file or directory: 'ppp'
item 0: -                     description: initial shape of u
item 1: t                     description: initial shape of H
item 2: s                     description: shape of u at time=2.5
curve1                description: initial shape of u
curve2                description: initial shape of H
curve3                description: shape of u at time=2.5
1.2 is greater than or equal to 100 (a is <type 'str'> and b is <type 'int'> )
1.2 is less than 100 (a is <type 'float'> and b is <type 'int'> )
testing string b < 100: error!
testing float(b) < 100: ok
{
   'a' : 4,
   'b' : {
       'c' : 'some',
       'd' : 77,
       },
   'e' : {
       'f' : {
           'g' : 1,
           'h' : 2,
           },
       'i' : 4,
       },
   },

double complex root=-1j
complex root=(66.0302268206-1.99862731095j), complex root=(-0.030226820552+0.998627310945j)
complex root=(2-2j), complex root=(-2+2j)
float root=2.0, float root=-2.0

regex:

[  just a string   with leading and trailing white space   ]
[just a string   with leading and trailing white space   ]
[just a string   with leading and trailing white space]

comments= []
comments= ['# a comment', '# another comment', '# third comment']
points2= [[ 0.          3.        ]
 [ 0.1         3.57883668]
 [ 0.2         4.03471218]
 [ 0.3         4.26407817]
 [ 0.4         4.19914721]
 [ 0.5         3.81859485]
 [ 0.6         3.15092636]
 [ 0.7         2.2699763 ]
 [ 0.8         1.28325171]
 [ 0.9         0.31495911]
 [ 1.         -0.51360499]
 [ 1.1        -1.10320415]
 [ 1.2        -1.39232922]
 [ 1.3        -1.36690931]
 [ 1.4        -1.06253328]
 [ 1.5        -0.558831  ]
 [ 1.6         0.03309841]
 [ 1.7         0.5882267 ]
 [ 1.8         0.98733573]
 [ 1.9         1.13583934]]
http://www.ifi.uio.no/~hpl/downloadme.dat
lines= ['Just a simple test for downloading files from the Internet.\n']
finished
CPU time of commontasks.py: 0.8 seconds on hplx30 i686, Linux

