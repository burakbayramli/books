function [simulatedata, H] = fattailed_garchsimulate(parameters,p,q,t,errors)
% PURPOSE:
%     FATTAILED_GARCH(P,Q) time series simulation
% 
% USAGE:
%     [simulatedata, H] = fattailed_garchsimulate(parameters,p,q,t,errors)
% 
% INPUTS:
%     parameters: a 1+p+q x 1 vector of inputs where p are ARCH coefs and Q are GARCH coefs
%     P: Positive, scalar integer representing a model order of the ARCH process  
%     Q: Non-Negative scalar integer representing a model order of the GARCH 
%         process: Q is the number of lags of the lagged conditional variances included
%         Can be empty([]) for ARCH process
%     t: Length of the time series desired
%     error:  The type of error being used, valid types are:
%             'NORMAL' - Gaussian Innovations
%             'STUDENTST' - T-distributed errors
%             'GED' - General Error Distribution
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


[r,c]=size(parameters);

if r<c
    parameters=parameters';
end


t=t+500;
if strcmp(errors,'NORMAL') | strcmp(errors,'STUDENTST') | strcmp(errors,'GED')
   if strcmp(errors,'NORMAL') 
      errortype = 1;
   elseif strcmp(errors,'STUDENTST') 
      errortype = 2;
   elseif strcmp(errors,'GED') 
      errortype = 3;
   end
else
   error('error must be one of the three strings NORMAL, STUDENTST, or GED');
end


constp=parameters(1);
archp=parameters(2:p+1);
garchp=parameters(p+2:p+q+1);
if errortype==2 | errortype==3
   nu=parameters(p+q+2);
   parameters=parameters(1:p+q+1);
end



if isempty(q)
   m=p;
else
   m=max(p,q);   
end


UncondStd =  sqrt(constp/(1-sum(archp)-sum(garchp)));
h=UncondStd.^2*ones(t+m,1);
data=UncondStd*ones(t+m,1);
if errortype==1
   RandomNums=randn(t+m,1);
elseif errortype==2
   RandomNums=stdtdis_rnd(t+m,nu);
else
   RandomNums=gedrnd(t+m,nu);
end


   
T=size(data,1);

for t = (m + 1):T
   h(t) = parameters' * [1 ; data(t-(1:p)).^2;  h(t-(1:q)) ];
   data(t)=RandomNums(t)*sqrt(h(t));
end

simulatedata=data(m+1+500:T);
H=h((m + 1+500):T);
