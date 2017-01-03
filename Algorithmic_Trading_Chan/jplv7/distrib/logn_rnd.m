function r = logn_rnd(mu,sigma,m,n);
% PURPOSE: random draws from the lognormal distribution
%---------------------------------------------------
% USAGE: rnd = logn_rnd(mu,sigma,m,n)
% where: mu = the mean (may be a matrix) 
%        sig = the standard deviation (may be a matrix)
%        m,n = the size of r in the case of mu or sig a matrix 
%---------------------------------------------------
% RETURNS:
%        rnd = a matrix of random numbers from the lognormal distribution      
% --------------------------------------------------
% SEE ALSO: logn_cdf, logn_pdf, logn_inv
%---------------------------------------------------
% NOTE: Copyright (c) 1993 by The MathWorks, Inc.
%---------------------------------------------------
% REFERENCES: 
%    Evans, Merran, Hastings, Nicholas and Peacock, Brian,
%    "Statistical Distributions, Second Edition", Wiley
%    1993 p. 102-105.
% -------------------------------------------------------------
% SEE ALSO: logn_d, logn_pdf, logn_inv, logn_rnd
% -------------------------------------------------------------


if nargin < 2, 
    error('Wrong # of arguments to logn_rnd');
end

errorcode = 0;

if nargin == 2
[murow mucol] = size(mu);
[sirow sicol] = size(sigma);
if (murow == 1 | sirow == 1)
rows = max(murow,sirow);
end;
if (mucol ==1 | sicol == 1)
columns = max(mucol,sicol);
end;
end

if nargin == 3
error('Wrong number of arguments to logn_rnd');
end

if nargin == 4
rows = m;
columns = n;
end


%Initialize r to zero.
r = zeros(rows, columns);

r = exp(randn(rows,columns) .* sigma + mu);

% Return NaN if SIGMA is not positive.
if any(any(sigma <= 0));
    if prod(size(sigma) == 1)
        r = NaN * ones(rows,columns);
    else
        k = find(sigma <= 0);
        r(k) = NaN * ones(size(k));
    end
end
