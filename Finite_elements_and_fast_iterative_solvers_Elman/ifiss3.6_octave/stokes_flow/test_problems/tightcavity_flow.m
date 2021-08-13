function [bcx,bcy] = specific_flow(xbd,ybd)
%tightcavity_flow   Reference problem 5.3 alternative inflow condition 
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies watertight cavity flow boundary condition
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bcx=0*xbd; bcy=0*xbd;
k=find(ybd==1 & xbd>-1 & xbd<1); bcx(k)=1;
return