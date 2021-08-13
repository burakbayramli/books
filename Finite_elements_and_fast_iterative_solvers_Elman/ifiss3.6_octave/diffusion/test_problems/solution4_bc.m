function bc = specific_bc(xbd,ybd)
%solution4_bc   Reference problem 1.4  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=(xbd.^2+ybd.^2).^(1/3) .*sin((pi+2*atan2(ybd,xbd))/3);
return