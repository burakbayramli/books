function [flowx,flowy] = specific_wind(x,y,nel)
%constant_wind   Reference problem 3.3 convective wind 
%   [flowx,flowy] = specific_wind(x,y,nel);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          nel        number of elements
%   specifies constant wind at angle theta to vertical
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      theta= -pi/6;
      flowx =  sin(theta)*ones(nel,1);       
      flowy =  cos(theta)*ones(nel,1);
      return