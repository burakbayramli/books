function [flowx,flowy] = specific_wind(x,y,nel)
%ref_wind   Reference problem 3.2 convective wind 
%   [flowx,flowy] = specific_wind(x,y,nel);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          nel        number of elements
%   specifies variable vertical wind.
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      flowy =  1+(x+1).*(x+1)/4;  flowx =  zeros(nel,1); 
      return