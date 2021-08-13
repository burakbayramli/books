function bc = specific_bc(xbd,ybd)
%solution3_bc   Reference problem 1.3  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=2*(1+ybd)./((ybd+1).*(ybd+1) + (xbd+3).*(xbd+3));
return