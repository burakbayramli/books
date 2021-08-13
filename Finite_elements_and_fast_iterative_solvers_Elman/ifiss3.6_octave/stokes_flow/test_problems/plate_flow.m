function [bcx,bcy] = specific_flow(xbd,ybd)
%plate_flow   Reference problem 7.4 inflow condition 
%   [bcx,bcy] = specific_flow(xbd,ybd);
%   input
%          xbd          x coordinate vector
%          ybd          y coordinate vector 
%
%   specifies Blasius flow boundary condition
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bcx=0*xbd; bcy=0*xbd;
k=find(xbd==-1); bcx(k)=1;
k=find(ybd==-1); bcx(k)=1;
k=find(ybd==1); bcx(k)=1;
return