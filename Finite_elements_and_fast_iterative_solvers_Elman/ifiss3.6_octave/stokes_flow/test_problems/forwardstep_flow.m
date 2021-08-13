function [bcx,bcy] = specific_flow(xbd,ybd)
%forwardstep_flow   Forward facing step inflow condition 
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies forward step flow boundary condition
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bcx=0*xbd; bcy=0*xbd;
k=find(xbd==5); bcx(k)=-0.5*(1+ybd(k)).*(1-ybd(k));
return