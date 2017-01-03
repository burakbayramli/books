function x = sor(A,b,n,w,z)
%
%   x = sor(A,b,n,w,z)
%    
%      SOR iteration on system A*x = b with printing
%      n -- number of iterations
%      w -- SOR parameter
%      z -- initial vector  (default 0)
%
%      x -- final iterate
%

if nargin <=4, z=0*b; end
[m,l] = size(A); 
D = diag(diag(A));
J = D\(D - A);
c = D\b;
x=z;
for k = 1:n
   for i=1:m
      x(i,1) = (1-w)*x(i,1) + w*(J(i,:) * x + c(i,1));
   end
   fprintf(1,'%3d     ',k)
   fprintf(1,'%5.4f     ',x')
   fprintf(1,'\n')
end
