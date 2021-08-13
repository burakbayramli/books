function f = specific_rhs(x,y,nel)
%zero_rhs   zero RHS forcing function
%   f = specific_rhs(x,y,nel)
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          nel        number of elements  
%   IFISS function: DJS; 17 January 2010. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
f=zeros(nel,1);
return