function nm = nmin(q,r,s)
% PURPOSE: determines the size of pilot sample needed by raftery.m function
% ------------------------------------------------          
% USAGE: nmin(q,r,s)
% where: q = quantile of the quantity of interest
%        r = level of precision desired
%        s = probability associated with r
% ------------------------------------------------
% RETURNS: 
%        nmin  = pilot sample size for raftery.m function
% ------------------------------------------------        
% NOTES: uses function ppnd for the cumulative normal
% ------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu
  
 phi = ppnd((s+1.0)/2);
 nm = fix(((1.0 - q)*q*phi^2/r^2) + 1.0);
 
 

