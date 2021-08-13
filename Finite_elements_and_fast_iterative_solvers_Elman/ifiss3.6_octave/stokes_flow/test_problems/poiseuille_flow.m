function [bcx,bcy] = specific_flow(xbd,ybd)
%poiseuille_flow   Reference problem 5.1 inflow condition 
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies Poiseuille flow boundary condition
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bcx=1-ybd.*ybd; bcy=0*xbd;
return