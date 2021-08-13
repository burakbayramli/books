function f = fint(x)
%FINT subdivision function 
%   f = fint(x);
%   called by fitint
%   IFISS function: DJS; 28 February 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      global global_N global_INTL global_LASTDL
      f=(global_INTL*(x-1.0)-global_LASTDL)*(x^global_N) + global_LASTDL;
%     fprintf('\n %10.4e   %10.4e',x,f)
      return
