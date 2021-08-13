function [flowx,flowy] = specific_wind(x,y,nel)
%vertical_wind   Reference problem 3.1 convective wind 
%   [flowx,flowy] = specific_wind(x,y,nel);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          nel        number of elements
%   specifies constant vertical wind.
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      flowx =  zeros(nel,1);  flowy =  ones(nel,1);
      return