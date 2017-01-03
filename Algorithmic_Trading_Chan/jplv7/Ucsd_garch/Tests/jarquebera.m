function results = jarquebera(data, k, alpha)
% PURPOSE:
%     Performs a Jarque-Bera test for normality.  Uses the skewness and kurtosis to determine if a
%     distribution is normal.  Has good power against skewness and kurtoisi problems, but little elsewhere
% 
% USAGE:
%     results = jarquebera(data, k, alpha)
% 
% INPUTS:
%     data     - A set of data from a presumed normal distribution
%     k        - the number of dependant variables if any used in creating the data(must be >= 2)(optional)
%     alpha    - The level of the test used for the hypothesis(H)(optional)
% 
% OUTPUTS:
%   results, a structure with fileds:
%         statistic - A scalar representing the statistic
%         pval      - A scalar pval of the null
%         H         - A hypothesis dummy(0 for fail to reject the null of normality, 1 otherwise)
% 
% COMMENTS:
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001



[t,c]=size(data);

if nargin <2
   k = 2;
end

if nargin < 3
   alpha = .05;
end


val = skewness(data)^2;
val = val + (1/4)*(kurtosis(data)-3)^2;
results.statistic = ((t-k)/6)* val;

results.pval = 1-chis_cdf(results.statistic,k);
results.H=results.pval<alpha;
