function [simulatedata, H] = dcc_univariate_simulate(parameters,p,q,data)
% PURPOSE:
%     Make a univariate time series of conditional variances for use by DCC_GARCH_FULL_LIKELIHOOD
% 
% 
% USAGE:
%    [simulatedata, H] = dcc_univariate_simulate(parameters,p,q,data)
% 
% 
% INPUTS:
%     parameters    - A vector(p+q+1) by 1 of parameters of form 
%                        [omega archp(1) ... archp(p) garchp(1) ... garchp(q)]
%     p             - The number of innovations to include(scalar)
%     q             - The length of the AR in the Garch process(scalar)
%     data          - A set of zero mean residuals you wish to introduce garch effects to
% 
% OUTPUTS:
%      simulatedata - The data that has had garch standard deviation applied to it
%      H            - The conditional variances for the data
% 
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001



constp=parameters(1);
archp=parameters(2:p+1);
garchp=parameters(p+2:p+q+1);



if isempty(q)
   m=p;
else
   m  =  max(p,q);   
end

[t,k]=size(data);
UncondStd =  sqrt(cov(data));
h=UncondStd.^2*ones(t+m,1);
data=[UncondStd*ones(m,1);data];
RandomNums=randn(t+m,1);
T=size(data,1);


h=garchcore(data,parameters,UncondStd^2,p,q,m,T);
% for t = (m + 1):T
%     h(t) = parameters' * [1 ; data(t-(1:p)).^2;  h(t-(1:q)) ];
% end




simulatedata=data((m+1):T);
H=h((m + 1):T);
