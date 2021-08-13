function [bcx,bcy] = specific_flow(xbd,ybd)
%symstep_flow   symmetric channel inflow condition
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies backward step flow boundary condition
%   IFISS function: DJS; 20 September 2016.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
bcx=0*xbd; bcy=0*xbd;
k=find(xbd==-1); bcx(k)=1-4*ybd(k).*ybd(k);
%%% bcx(k)= bcx(k) + 0.01*sin(2*pi*ybd(k)); % skew inlet profile
return
