%EXPOWER demonstrates the power method of finding the dominant eigenvalue

% Written by T. Bryan on 27 August 1993

echo on
clc
% The power method finds the dominant eigenvalue of a matrix (if there is
% one) by starting with an initial vector v and repeating the command:
%      v = A*v; v = v/norm(v);
%
% If successful the method converges to the dominant eigenvalue,
% which will be lambda = v'*A*v
%
% The power method can fail if A has two eigenvalues of the same magnitude.
% The starting vector v needs to have a component along the dominant eigenvector  
%
% We give examples of the method by calling the routine "power".
% OPEN A WINDOW to see the plots drawn by power

pause % Strike any key to continue
clc
A = [1 0;0.005 3];

v = [1; 0];

pause(3)
powers(A,v,10);

% Actual eigenvalues of A
eig(A)
% The method converged

pause % Strike any key to continue

clc
A = [1 0; 0.005 -1.05];

v = [1;0];

pause(3)
powers(A,v,10);

% Actual eigenvectors and eigenvalues of A
[V,D] = eig(A)
% The method failed. The starting vector v had no component 
% in the direction of the dominant eigenvector [0 1]'.

pause % Strike any key to continue

clc
A = [1 0; 0 -1];

v = [1; 1];

pause(3)
powers(A,v,10);

% Actual eigenvalues of A
eig(A)
%The method failed. A has two eigenvalues of equal size.

pause % Strike any key to continue

clc
c = cos(.2);
s = sin(.2);
A = [c s;-s  c];
v = [1; 0];
pause(3)
powers(A,v,30);

% Actual eigenvalues of A
eig(A)
% The method failed. A has imaginary eigenvalues of equal size.

echo off

