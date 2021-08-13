function bc = specific_bc(xbd,ybd)
%collide_bc   Reference problem 5.4 boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with colliding flow
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=-(xbd.^5)+5*xbd.*ybd.^4;
return