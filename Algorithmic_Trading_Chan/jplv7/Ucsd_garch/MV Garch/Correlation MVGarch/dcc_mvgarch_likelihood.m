function [logL, Rt, likelihoods, Qt]=dcc_mvgarch_likelihood(params, stdresid, P, Q)
% PURPOSE:
%        Restricted likelihood for use in the DCC_MVGARCH estimation and
%        returns the likelihood of the 2SQMLE estimates of the DCC parameters
% 
% USAGE:
%        [logL, Rt, likelihoods]=dcc_garch_likelihood(params, stdresid, P, Q)
% 
% INPUTS:
%    params      - A P+Q by 1 vector of parameters of the form [dccPparameters;dccQparameters]
%    stdresid    - A matrix, t x k of residuals standardized by their conditional standard deviation
%    P           - The innovation order of the DCC Garch process
%    Q           - The AR order of the DCC estimator
% 
% OUTPUTS:
%    logL        - Th ecalculate Quasi-Likelihood
%    Rt          - a k x k x t 3 dimesnaional array of conditional correlations
%    likelihoods - a t by 1 vector of quasi likelihoods
% 
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001



[t,k]=size(stdresid);
a=params(1:P);
b=params(P+1:P+Q);
sumA=sum(a);
sumB=sum(b);

%First compute Qbar, the unconditional Correlation Matrix
Qbar=cov(stdresid);

% Next compute Qt
m=max(P,Q);
Qt=zeros(k,k,t+m);
Rt=zeros(k,k,t+m);
Qt(:,:,1:m)=repmat(Qbar,[1 1 m]);
Rt(:,:,1:m)=repmat(Qbar,[1 1 m]);
logL=0;
likelihoods=zeros(1,t+m);
%The stdresid have epected value 1  maybe but in the variances
stdresid=[zeros(m,k);stdresid];
for j=(m+1):t+m
   Qt(:,:,j)=Qbar*(1-sumA-sumB);   
   for i=1:P
     Qt(:,:,j)=Qt(:,:,j)+a(i)*(stdresid(j-i,:)'*stdresid(j-i,:));
   end
   for i=1:Q
      Qt(:,:,j)=Qt(:,:,j)+b(i)*Qt(:,:,j-i);
   end
   Rt(:,:,j)=Qt(:,:,j)./(sqrt(diag(Qt(:,:,j)))*sqrt(diag(Qt(:,:,j)))');
   likelihoods(j)=log(det(Rt(:,:,j)))+stdresid(j,:)*inv(Rt(:,:,j))*stdresid(j,:)';
   logL=logL+likelihoods(j);
end;

Qt=Qt(:,:,(m+1:t+m));
Rt=Rt(:,:,(m+1:t+m));
logL=(1/2)*logL;
likelihoods=(1/2)*likelihoods(m+1:t+m);

if isreal(logL) 
else
   disp('Imag')
   params
   logL=10E+8;
end

if isinf(logL)
   disp('Inf')
   params
   logL=10E+8;
end
