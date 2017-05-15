% Some mathematics (1)
%3x3 example
A=[1 3 1;
   2 5 2;
   1 3 1];

[u,s,v]=svd(A);

A1=s(1,1)*u(:,1)*v(:,1)';
A2=s(2,2)*u(:,2)*v(:,2)';
A3=s(3,3)*u(:,3)*v(:,3)';

Ap=A1+A2+A3;

M=A*A';
N=A'*A;

[U,D1]=eig(M);
S1=sqrt(diag(D1));

[V,D2]=eig(N);
S2=sqrt(diag(D2));

%check values of the matrices
s
A1
