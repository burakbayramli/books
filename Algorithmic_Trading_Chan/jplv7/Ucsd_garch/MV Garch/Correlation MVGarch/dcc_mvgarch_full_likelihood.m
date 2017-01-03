function [logL, Rt, likelihoods, Qt]=dcc_garch_full_likelihood(parameters, data, archP,garchQ,dccP,dccQ)
% PURPOSE:
%        Full likelihood for use in the DCC_MVGARCH estimation and
%        returns the likelihood of the QMLE estimates of the DCC parameters
% 
% USAGE:
%        [logL, Rt, likelihoods]=dcc_garch_full_likelihood(parameters, data, archP,garchQ,dccP,dccQ)
% 
% INPUTS:
%    parameters  - A k+sum(archP)+sum(garchQ)+dccP+dccQ vector of parameters of the form
%                  [GarchParams(1) GarchParams(2) ... GarchParams(k) DCCParams]
%                  where the garch parameters from each estimation are of the form
%                  [omega(i) alpha(i1) alpha(i2) ... alpha(ip(i)) beta(i1) beta(i2) ... beta(iq(i))]
%                  and DCCparams are [DCCa DCCb]
%    data        - A t by k matrix of zero mean residuals
%    archP       - A vector of arch innovation lag lengths
%    garchQ      - A vector of Garch AR lag lengths
%    dccP        - A scalar of the DCC innovations lag length
%    dccQ        - A scalar of the DCC AR lag lengths
% 
% OUTPUTS:
%    logL        - The estimated log likelihood
%    Rt          - The estimates covariances
%    likelihoods - The likelihoods (t by 1)
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


% First we need to make the T by K matrix of variances.
[t,k]=size(data);
index=1;
H=zeros(size(data));

for i=1:k
    univariateparameters=parameters(index:index+archP(i)+garchQ(i));
    [simulatedata, H(:,i)] = dcc_univariate_simulate(univariateparameters,archP(i),garchQ(i),data(:,i));
    index=index+1+archP(i)+garchQ(i);
end

stdresid=data./sqrt(H);
Qbar=cov(stdresid);

stdresid=[ones(max(dccP,dccQ),k);stdresid];
a=parameters(index:index+dccP-1);
b=parameters(index+dccP:index+dccP+dccQ-1);
sumA=sum(a);
sumB=sum(b);

%First compute Qbar, the unconditional Correlation Matrix


% Next compute Qt
m=max(dccP,dccQ);

Qt=zeros(k,k,t+m);
Qt(:,:,1:m)=repmat(Qbar,[1 1 m]);
Rt=zeros(k,k,t+m);


logL=0;
likelihoods=zeros(t+m,1);
H=[zeros(m,k);H];
P=dccP;
Q=dccQ;
for j=(m+1):t+m
   Qt(:,:,j)=Qbar*(1-sumA-sumB);
   for i=1:P
      Qt(:,:,j)=Qt(:,:,j)+a(i)*(stdresid(j-i,:)'*stdresid(j-i,:));
   end
   for i=1:Q
      Qt(:,:,j)=Qt(:,:,j)+b(i)*Qt(:,:,j-i);
   end
   Rt(:,:,j)=Qt(:,:,j)./(sqrt(diag(Qt(:,:,j)))*sqrt(diag(Qt(:,:,j)))');
   likelihoods(j)=k*log(2*pi)+sum(log(H(j,:)))+log(det(Rt(:,:,j)))+stdresid(j,:)*inv(Rt(:,:,j))*stdresid(j,:)';
   logL=logL+likelihoods(j);
end;
Rt=Rt(:,:,(m+1:t+m));
logL=(1/2)*logL;
likelihoods=(1/2)*likelihoods(m+1:t+m);
