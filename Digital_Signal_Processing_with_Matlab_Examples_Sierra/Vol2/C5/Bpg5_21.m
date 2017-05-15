% Some mathematics (4)
% Example of QR factorization

%normal equation: (A'*A)*x = A'*b

A=[5.2 7.7 2.6;
   3.4 1.1 0.7;
   4.2 6.3 5.8;];
b= [3 2 1]';

%QR factorization
[Q,R]=qr(A,0);

V=Q'*b; %right-hand side of equation

%problem  solution (value of x) 
x= R\V;

%print values
x
Q
R