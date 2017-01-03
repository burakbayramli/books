function [simulatedata, H] = garchsimulate(t,parameters,p,q)
% PURPOSE:
%     GARCH(P,Q) time series simulation
% 
% USAGE:
%     [simulatedata, H] = garchsimulate(t,parameters,p,q)
% 
% INPUTS:
%     t: Length of the time series desired
%     parameters: a 1+p+q x 1 vector of inputs where p are ARCH coefs and Q are GARCH coefs
%     P: Positive, scalar integer representing a model order of the ARCH process
%     Q: Non-Negative scalar integer representing a model order of the GARCH 
%        process: Q is the number of lags of the lagged conditional variances included
%        Can be empty([]) for ARCH process
% 
% OUTPUTS:
%     simulatedata: A time series with GARCH variances and normal disturbances
%     H:  A vector of conditional variances used in making the time series
% 
% COMMENTS:
%   The time-conditional variance, H(t), of a GARCH(P,Q) process is modeled 
%   as follows:
%
%     H(t) = Omega + Alpha(1)*r_{t-1}^2 + Alpha(2)*r_{t-2}^2 +...+ Alpha(P)*r_{t-p}^2+...
%                    Beta(1)*H(t-1)+ Beta(2)*H(t-2)+...+ Beta(Q)*H(t-q)
%
% NOTE: This program generates 500 more than required to minimize any starting bias
%
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


constp=parameters(1);
archp=parameters(2:p+1);
garchp=parameters(p+2:p+q+1);

if any(parameters<0)
    error('No negative parameters');
end

[r,c]=size(parameters);

if r<c
    parameters=parameters';
end

if isempty(q)
   m=p;
else
   m  =  max(p,q);   
end

t=t+500;
UncondStd =  sqrt(constp/(1-sum(archp)-sum(garchp)));
h=UncondStd.^2*ones(t+m,1);
data=UncondStd*ones(t+m,1);
RandomNums=randn(t+m,1);
T=size(data,1);


for t = (m + 1):T
   h(t) = parameters' * [1 ; data(t-(1:p)).^2;  h(t-(1:q)) ];
   data(t)=RandomNums(t)*sqrt(h(t));
end
simulatedata=data((m+1+500):T);
H=h(m+1+500:T);
