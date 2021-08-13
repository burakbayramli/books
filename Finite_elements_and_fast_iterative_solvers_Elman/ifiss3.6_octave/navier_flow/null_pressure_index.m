function n_null = null_pressure_index(domain,qmethod,np)
%NULL_PRESSURE_INDEX index associated with the pressure nullspace
%   n_null = null_pressure_index(domain,qmethod,np);
%   input
%          domain     domain index
%          qmethod    mixed method 
%          np         pressure space dimension
%
%   IFISS function: DJS; 12 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
n_null = 0;
if domain==1,
   if qmethod==3, n_null = np-2;
   else           n_null = np;
   end
end
