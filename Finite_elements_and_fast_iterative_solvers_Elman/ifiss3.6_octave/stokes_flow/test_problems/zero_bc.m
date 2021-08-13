function bc = specific_bc(xbd,ybd)
%zero_bc   Reference problems 5.3 and 5.4 boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with enclosed flow
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=0*xbd;
return