function [lmstat, lmprob, reslm] = lmtest(resultr,xu)
% PURPOSE: computes LM-test for two regressions
%---------------------------------------------------
% USAGE: [lmstat lmprob, result] = lmtest(resultr,xmatrixu)
% or:    lmtest(resultr,xmatrixu), which prints output to the screen
% 
% Where: resultr  = matrix returned by ols() for restricted regression
%        xmatrixu = explanatory variables matrix from unrestricted model
%---------------------------------------------------
% RETURNS: lmstat = calculated chi-squared statistic 
%          lmprob = marginal probability for lmstat
%          result = ols() matrix for printing with prt(result)
%---------------------------------------------------        
% NOTE:   Expected value of (lmstat) = #restrictions
%---------------------------------------------------
% SEE ALSO: ols() waldf()
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

pflag = 0;
if nargout == 0
pflag = 1;
elseif nargin ~= 2 % flag incorrect arguments
    error('lm_test: Wrong # of input arguments');
elseif isstruct(resultr) == 0
 error('lm_test requires an ols results structure');
end;
nobs1 = resultr.nobs;   [nobs rsize] = size(xu);
if nobs1 ~= nobs
 error('lm_test: inputs resultr and xu should have same # of obs');
end;
er = resultr.resid; % recover residuals from restricted model
% step 1) regress er on all the x's (contained in xu-matrix)
 reslm = ols(er,xu);
% step 2) recover R^2 and compute chi-squared statistic
 rsqr = reslm.rsqr;       lmstat = rsqr*nobs;
% need to figure the # of restrictions
[junk ku] = size(xu); kr = resultr.nvar; nrestrict = ku - kr;
lmprob = 1.0 - chis_prb(lmstat,nrestrict);
if pflag == 1
fprintf(1,'LM-statistic     = %16.8f \n',lmstat);
fprintf(1,'probability      = %16.4f \n',lmprob);
fprintf(1,'# restrictions   = %8d    \n',nrestrict);
end;
