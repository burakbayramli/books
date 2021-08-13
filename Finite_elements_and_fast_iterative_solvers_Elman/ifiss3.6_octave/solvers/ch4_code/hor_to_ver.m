function [Aver,p,ip] = hor_to_ver(Ahor)
%hor_to_ver   reorder matrix from horizontal to vertical 
%   [Aver,p,ip] = hor_to_ver(Ahor)
%   input
%          Ahor    matrix from horizontal ordering of square grid 
%   output
%          Aver    matrix from vertical ordering of square grid
%          p       horizontal to vertical permutation, xv = x(ip)
%          ip      vertical to horizontal permutation, y = yv(p)
%
%   IFISS function: HCE; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 

%To multiply a vector x by Aver:  
%   xv = x(ip), yv = Aver*xv, y = yv(p);

N = length(Ahor);
n = sqrt(N);
p  = zeros(N,1);
ip = zeros(N,1);

for j=1:n,
   for i=1:n,
      hor = (j-1)*n+i;
      ver = (i-1)*n+j;
      ip(ver) = hor;
      p(hor) = ver;
   end
end

Aver = Ahor(ip,ip);