function [bcx,bcy] = specific_flow(xbd,ybd)
%backwardstep_flow   Reference problem 5.2 inflow condition 
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies backward step flow boundary condition
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bcx=0*xbd; bcy=0*xbd;
k=find(xbd==-1); bcx(k)=4*ybd(k).*(1-ybd(k));
return