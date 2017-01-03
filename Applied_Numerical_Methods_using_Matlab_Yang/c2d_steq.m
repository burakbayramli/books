function [Ad,Bd]=c2d_steq(A,B,T,N)
if nargin<4, N=100; end
I= eye(size(A,2));  PSI= I;
for m=N:-1:1, PSI= I +A*PSI*T/(m+1); end
Ad= I +A*PSI*T;  Bd= PSI*T*B;