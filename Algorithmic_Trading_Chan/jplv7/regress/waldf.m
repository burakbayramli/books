function [fstat, fprb] = waldf(resultr,resultu)
% PURPOSE: computes Wald F-test for two regressions
%---------------------------------------------------
% USAGE: [fstat fprob] = waldf(resultr,resultu)
%    or: waldf(resultr,resultu), which prints to the screen
% Where: resultr = results structure from ols() restricted regression
%        resultu = results structure from ols() unrestrcted regression
%---------------------------------------------------
% RETURNS: fstat = {(essr - essu)/#restrict}/{essu/(nobs-nvar)} 
%          fprb  = marginal probability for fstat
% NOTE:  large fstat => reject the restrictions as inconsisent 
%                       with the data
%---------------------------------------------------
% SEE ALSO: ols()
%---------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com

pflag = 0;
if nargout == 0
pflag = 1;
elseif nargin ~= 2 % flag incorrect arguments
    error('waldf: Wrong # of input arguments');
elseif isstruct(resultu) == 0
 error('waldf requires an ols results structure as input');
elseif isstruct(resultr) == 0
 error('waldf requires an ols results structure as input');
end;
% get nobs, nvar from unrestricted and restricted regressions
nu = resultu.nobs;  nr = resultr.nobs;
ku = resultu.nvar;  kr = resultr.nvar;
if nu ~= nr
 error('waldf: the # of obs in the results structures are different');
end;
if (ku - kr) < 0 % flag reversed input arguments
error('waldf: negative dof, check for reversed input arguments');
end;
% recover residual sum of squares from .sige field of the result structure
epeu = resultu.sige*(nu-ku);  eper = resultr.sige*(nr-kr);
numr = ku - kr; % find # of restrictions
ddof = nu-ku;   % find denominator dof
fstat1 = (eper - epeu)/numr; % numerator
fstat2 = epeu/(nu-ku);       % denominator
fstat = fstat1/fstat2; fprb = fdis_prb(fstat,numr,ddof);

if pflag == 1
fprintf(1,'Wald F-statistic = %16.8f \n',fstat);
fprintf(1,'probability      = %16.4f \n',fprb);
fprintf(1,'num,denom dof    = %4d,%4d\n',numr,(nu-ku));
end;
