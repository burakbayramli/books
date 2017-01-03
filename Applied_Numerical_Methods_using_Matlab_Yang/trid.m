function x=trid(A,b)
% solve tridiagonal system of equations
%  Upper Triangularization
N= size(A,2); row=0;
if size(b,1)==1, b=b(:); row=1; end
for m =2:N
  tmp =A(m,m-1)/A(m-1,m-1);
  A(m,m) =A(m,m) -A(m-1,m)*tmp; A(m,m-1) =0;
  b(m,:) =b(m,:) -b(m-1,:)*tmp;
end
% Back Substitution
x(N,:) =b(N,:)/A(N,N);
for m =N-1: -1: 1
  x(m,:) =(b(m,:) -A(m,m+1)*x(m+1))/A(m,m);
end
if row==1, x=x'; end