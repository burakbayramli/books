function f = plate_rhs(x,y,nel)
%plate_rhs   load function
%   f = plate_rhs(x,y,nel)
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          nel        number of elements
%
%   IFISS function: DJS; 5 September 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
f=ones(nel,1);
%f=x.*x+y.*y;
return
