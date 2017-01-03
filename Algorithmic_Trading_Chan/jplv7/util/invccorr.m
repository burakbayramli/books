function [COEF] = invccorr(COEF,MVEC,SVEC)
% PURPOSE: converts matrix to correlation form with
%          unit normal scaling.
%---------------------------------------------------
% USAGE: coef = invccorr(coef,mvec,svec)
% where: coef = a vector from regression using correlation
%               standardized data
%        mvec = original means (returned by ccorr1,ccorr2)
%        svec = original std deviations  (returned by ccorr1,ccorr2)
%---------------------------------------------------
% RETURNS:  coef = coefficients in raw form         
% --------------------------------------------------
% SEE ALSO: ccorr1, ccorr2
%---------------------------------------------------
% References: Montgomery & Peck p. 155

% written by:
% M J Chlond - Nov94
% m.chlond@uclan.ac.uk

% documentation modified by J.P. LeSage

m = length(COEF);

slopes = COEF(2:m) .* (SVEC(1) ./ SVEC(2:m))';
intcpt = MVEC(1) - sum(slopes .* MVEC(2:m)');
COEF   = [intcpt;slopes];

