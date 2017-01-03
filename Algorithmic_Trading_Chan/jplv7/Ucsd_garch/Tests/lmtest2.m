function results = lmtest2(stdresid,q)
% PURPOSE:
%  Performs an Engle LM test of the squarred residuals regressed on q lags
%  and a constant.  Under the null of no serial correlation, this is asymptotically 
%  distributed X2(q)
% 
% USAGE:
%  results = lmtest2(stdresid,q)
% 
% 
% INPUTS:
%  stdresid - A set of deviates from a garch process OR a ser of deviates suspected of having GARCH
%  q-         The maximum number of lags to regress on.  The statistic and pval will be returned for all sets of 
%             lagged squarrd residuals up to and including q
% 
% OUTPUTS:
%  results, a structure with fields:
%  statistic - A Qx1 vector of statistics
%  pval      - A Qx1 set of appropriate pvals
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

statistic=zeros(q,1);
pval=zeros(q,1);

sqresid=stdresid.^2;

for i=1:q
   [y,x]=newlagmatrix(sqresid,i,1);
   beta = x\y;
   rsquared=1-((y-x*beta)'*(y-x*beta)/((y-mean(y))'*(y-mean(y))));
   statistic(i)=length(y)*rsquared;
end
results.statistic=statistic;
results.pval=1-chis_cdf(statistic,(1:q)');

