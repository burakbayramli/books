function [bcx,bcy] = specific_flow(xbd,ybd)
%obstacle_flow   Obstacle problem inflow condition 
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies flow over obstacle boundary condition
%   IFISS function: HCE; 1 July 2006.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bcx=0*xbd; bcy=0*xbd;
k=find(xbd==0); bcx(k)=(1-ybd(k)).*(1+ybd(k));
return