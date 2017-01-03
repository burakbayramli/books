echo on
clc
% The inverse of A is here computed by Gauss-Jordan elimination on [A I].
% The ref (row echelon form) of this block matrix is [I inverse(A)],
% provided A is invertible. Then the program picks out that second block.
%  
%    An unusual inverse appeared in the LU factorization of A = ones + eye.
% Choose size n = 5:

A = ones(5) + eye(5);
[L,U] = slu(A);
L
LINV = inverse(L)

% The inverse is lower triangular -- of course. It has minus signs below the
% diagonal. LINV has simple fractions in rows where L has those same fractions
% in columns. Multiply L*LINV or LINV*L by hand to see I appear.
%  
% Experiment 1: Try the same construction to find L and LINV starting with
% B = ones + 2*eye and C = ones + 4*eye.
%  
% Experiment 2: Compute and explain inverse(A) for other sizes n. 
% There must be a general formula for these inverses.
%  
% Experiment 3: Find a number K so that D = ones(5) - K*eye(5) has NO INVERSE.
% Which vector x solves Dx = 0?
% press any key
pause
clc
%  
%  Now look at the inverse of A. This should equal inverse (L*U)

inverse(A)
inverse(U)*inverse(L)

% Problem 1A: Why does a symmetric invertible matrix have a symmetric inverse?
%  
% Problem 1B: Does the inverse of the transpose equal the transpose 
% of the inverse? Try a random R:

R = rand(4);
inverse(R') - inverse(R)'

% press any key
pause
clc
% Suppose R is a random 4 by 3 matrix. Its rank is probably 3. Its rows must
% be dependent. Its columns are probably independent. There is a left-inverse
% so that X*R = I (what size is I?). One formula is X = inverse(R'*R)*R'.
% There is no right-inverse matrix such that R*Y = I.  R*X is a projection.

R = rand(4,3)
X = inverse(R'*R)*R'
X*R
R*X

% How can you check that this is a projection matrix? Use math or MATLAB.
echo off
