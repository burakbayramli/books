echo on
clc
% Cramer's Rule solves Ax = b by determinants. The j-th component of the
% solution x is det B / det A. This matrix B is the same as A except the 
% right side vector b is placed in column j.

A = ones(5) + eye(5)
D = determ(A)
b = [1 0 0 0 0]';
x = cramer(A,b);
rats(x)

% You see  det A = 6 in the denominators of x.
% press any key
pause
clc
% Because of the special right side b = [1 0 0 0 0]', the solution x
% must be column 1 of A inverse. We check this now.

C = inverse(A);
rats(C)

% Next we do Cramer's Rule in detail by putting b into column 5 of A.
% Then component 5 of x is the ratio of det B to det A.
B = [A(:, 1:4) b]
T = determ(B)
rats(T/D)
x = cramer(A,b);
rats(x)
echo off
