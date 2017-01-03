function x = miter(T,c,n,z)
%
%   x = miter(T,c,n,z)
%    
%      Iteration of x = T*x + c with printing
%      n -- number of iterations
%      z -- initial vector  (default 0)
%
%      x -- final iterate
%

if nargin <=3, z=0*c; end

x=z;
for k = 1:n
   x = T*x + c;
   fprintf(1,'%3d     ',k)
   fprintf(1,'%5.4f     ',x')
   fprintf(1,'\n')
end
