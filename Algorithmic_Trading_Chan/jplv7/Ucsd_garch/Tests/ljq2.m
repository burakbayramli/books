function results = ljq2(residuals,q)
% PURPOSE:
%  Useful in looking at the squarred redisuals of a garch process
%  Performs an Ljung-Box Q test on the squared residuals 
%
% USAGE:
% [statistic, pval] = ljq2(residuals,q)
% 
% INPUTS:
%  residuals- A set of deviates from a garch process OR a ser of deviates suspected of having GARCH
%  q-         The maximum lag to examine the autocorrelation in squares
% 
% OUTPUTS:
%  results, a structure with fields:
%  statistic - A Qx1 vector of statistics
%  pval      - A Qx1 set of appropriate pvals
% 
% COMMENTS:
%  This was adapted from qstat2 in the JPL toolkit
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

p=1:q;
pval=zeros(q,1);
statistic=zeros(q,1);
sqresid=residuals.^2;

if (nargin ~= 2)    error('Wrong number of arguments to qstat'); end;  

n = rows(residuals); 
for i=1:q; 
    rho = sacf(sqresid,i,1); 
    rho2 = rho .* rho; 
    statistic(i) = n*(n+2)*sumc((rho2 ./ seqa(n-1,-1,p(i)))); 
end;   
results.statistic=statistic;
results.pval= 1-chis_cdf(statistic,p'); 