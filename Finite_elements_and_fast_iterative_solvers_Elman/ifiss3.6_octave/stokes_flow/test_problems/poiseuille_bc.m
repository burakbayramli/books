function bc = specific_bc(xbd,ybd)
%poiseuille_bc   Reference problem 5.1 boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with Poiseuille flow
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=ybd.*(1-ybd.*ybd/3)+(2/3);
return