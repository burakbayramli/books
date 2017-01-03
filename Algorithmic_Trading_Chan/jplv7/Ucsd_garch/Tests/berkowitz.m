function [statistic, pvalue, transformed_x, H]=berkowitz(x,size,type,dist,varargin)
% PURPOSE:
%  Performs a Kolmogorov-Smirnov test using the Berkowitz transform to a univariate normal
%  that the data are from a specified distribution
% 
% USAGE:
%   [statistic, pvalue, H]=berkowitz(x,size,type,dist,varargin)
% 
% 
% INPUTS:
%  x        -  A set of deviates from a garch process OR a ser of deviates suspected of having GARCH
%  size     -  The size for the test.  The test is a two tailed test with Size/2 probability in each tail.
%  dist     -  A char string of the name of the CDF, i.e. 'normcdf' for the normal, also use 'unifcdf' if you want
%              to only pass already probability integral transformed data to the funtion, and set the varargin at to 0,1.
%  type     -  A char string, either 'CS' if the data are crosectional or 'TS' for timeseries.  The TS chacks for autocorrelation 
%              in the prob integral transforms while the CS doesn't
%  varargin -  Arguements passed to the CDF, such as D.F. for a T-dist
% 
% 
% OUTPUTS:
%  statistic     - The Berkowitz statistic derived as a likelihood ratio of normal likelihoods
%  pval          - The asymptotic probability of signifigance
%  transformed_x - The transformed to normal data
%  H             - 1 for reject the null that the distribution is correct, 0 otherwise
% 
% 
% COMMENTS:
%  Note:  Is you want to pass in already probability inegral transformed data by the suspect distribtion
%         use the uniform distribution with parameters 0 1
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

 
cdfvals=feval(dist,x,varargin{:});
workingdata=norm_inv(cdfvals);
n=length(x);

if strcmp(type,'CS')
    avg=mean(workingdata);
    variance=cov(workingdata);
    restrictedLL=sum(log(norm_pdf(workingdata)))
    unrestrictedLL=sum(log(norm_pdf(workingdata,ones(n,1)*avg,ones(n,1)*variance)))
    statistic=-2*(restrictedLL-unrestrictedLL)
    pvalue=1-chis_cdf(statistic,2);
    H=pvalue<size;
else
    [y,lags]=newlagmatrix(workingdata,1,1);
    tempresult=ols(y,lags);
    newdata=tempresult.resid;
    variance=cov(newdata);          
    restrictedLL=sum(log(norm_pdf(workingdata(2:n))));
    unrestrictedLL=sum(log(norm_pdf(newdata,ones(n,1)*0,ones(n,1)*variance)));
    statistic=-2*(restrictedLL-unrestrictedLL);
    pvalue=1-chis_cdf(statistic,3);
    H=pvalue<size;
end;
transformed_x=workingdata;