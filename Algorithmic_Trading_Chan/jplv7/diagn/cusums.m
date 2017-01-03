function result = cusums(y,x)
% PURPOSE: computes cusum-squares test 
% ---------------------------------------
% USAGE: result = cusums(y,x)
% where:      y = dependent variable (n x 1)
%             x = independent variable matrix (n x k)
% ---------------------------------------
% RETURNS: results structure:
%          results.meth     = 'cusums'
%          results.rres     = recursive residuals
%          results.cusums   = cumulative sum of rres
%          results.upper95  = upper 95% confidence level
%          results.lower95  = lower 95% confidence level
%          results.nvar     = # of variables in x-matrix
% ----------------------------------------
% SEE ALSO: rec_resid, plt
% ----------------------------------------
% REFERENCES: Brown, Durbin, Evans 1975 J. Royal Statistical
% Society, 'Techniques for testing the constancy of regression
% relationships over time'', Series B, pp. 149-192.

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatia-econometrics.com

if nargin ~= 2
error('Wrong # of arguments to cusums');
end;

[n k] = size(x);

% use rec_resid function to get recursive residuals
vv = recresid(y,x);
sum2 = sum(vv.^2);

cusums = zeros(n,1);

i = k+1;
while i <=n
cusums(i,1) = sum(vv(k+1:i,1).^2);
i = i+1;
end;


cusums = cusums/sum2;

% construct bounds for test using 5 percent significance level
ndf = n-k;
inum = 1:n;
wu = .948*sqrt(ndf) + 2*.948*inum/sqrt(ndf);
wl = -wu;
wwu = .32894 + inum/ndf;
wwl = -.32894 + inum/ndf;


result.rres = vv;
result.cusums = cusums;
result.upper95 = wwu';
result.lower95 = wwl';
result.meth = 'cusums';
result.nvar = k;
