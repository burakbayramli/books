% MIT 18.335 - Lecture 8 MATLAB Demo 1
% Floating Point Arithmetic
% Per-Olof Persson, October 3, 2007

format long

% Overflow/underflow
1
-1
1e100
1e-100
1e400
1e-400

% Cancellation
x=rand
y=rand
z=x-y

x1=x+1e10
y1=y+1e10
z1=x1-y1

z1-z

% Epsilon
1+1e-20
(1+1e-20)-1
1+1e-16
1+2e-16
(1+2e-16)-1

e=1;
while (1+e>1) e=e/2, end % Don't optimize!
eps
b=2^50
(b+e*b)-b

% Signed zeros
0
+0
-0

% Infinity
1/0
-1/0
0/0

inf
1/inf
-1/inf
-1/-inf

2*inf
inf+inf
inf^inf

% NaN
inf-inf
inf/inf
0/0
nan+123

% Check for NaN
x=nan;
x==nan
x==x

isnan([1,2,3,nan,inf])
isinf([1,2,3,nan,inf])

% Round to even
e=eps/2
1+e
1+2*e
((1+2*e)-1)/e
((1+3*e)-1)/e
((1+4*e)-1)/e
[0:16; ((1+(0:16)*e)-1)/e]'

% View hex/bin representations
format hex

0
-0
inf
-inf
nan
-nan
123123+nan
1
2
(1:10)'

format short
xs=[0,-0,inf,-inf,nan,-nan,1:10,1+(0:10)*2^-23,2-(10:-1:0)*2^-23];
for x=xs
  fprintf('%10.8g %s\n',x,num2bin(single(x),true));
  pause
end
