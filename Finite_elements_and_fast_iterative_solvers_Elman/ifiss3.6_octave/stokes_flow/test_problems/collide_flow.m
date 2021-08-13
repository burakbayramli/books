function [bcx,bcy] = specific_flow(xbd,ybd)
%collide_flow   Reference problem 5.4 inflow condition 
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies colliding flow boundary condition
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bcx=20*xbd.*(ybd.^3); bcy=5*(xbd.^4)-5*(ybd.^4);
return