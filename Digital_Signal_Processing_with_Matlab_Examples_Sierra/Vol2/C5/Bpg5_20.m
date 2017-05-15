% Some mathematics (3)
% Example of Cholesky factorization

%normal equation: (A'*A)*x = A'*b
%it will become: M*x=r

A=[5.2 7.7 2.6;
   3.4 1.1 0.7;
   4.2 6.3 5.8;];
b= [3 2 1]';

M=A'*A;
r=A'*b;

%Cholesky factorization
C=chol(M);

%problem  solution (value of x)   
x= C\(C'\r);

%print values
x
C