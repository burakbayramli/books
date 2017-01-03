function [cmat,mvec,svec] = ccorr2(x)
% PURPOSE: converts matrix to correlation form with unit length scaling.
%---------------------------------------------------
% USAGE: [cmat mvec svec] = ccorr2(x)
% where: x = a vector
%---------------------------------------------------
% RETURNS:  cmat = correlation matrix
%           mvec - vector of means
%           svec - vector of standard deviations            
% --------------------------------------------------
% SEE ALSO: ccorr1, invcorr
%---------------------------------------------------
% References: Montgomery & Peck p. 155

% written by:
% M J Chlond - Nov94
% m.chlond@uclan.ac.uk

% documentation modified by J.P. LeSage

[n,m] = size(x);
mvec  = mean(x);
svec  = std(x);
cmat = [];
for i = 1:n
    cmat = [ cmat; (x(i,:)-mvec) ./ (svec .^ 2 * (n-1)) .^ .5];
end

