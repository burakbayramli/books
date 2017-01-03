function [logL, likelihoods]=cc_mvgarch_full_likelihood(parameters, data, archP,garchQ);
% PURPOSE:
%        Full likelihood for use in the CC_MVGARCH estimation and
%        returns the likelihood of the QMLE estimates of the CC parameters
% 
% USAGE:
%        [logL, likelihoods]=cc_mvgarch_full_likelihood(parameters, data, archP,garchQ);
% 
% INPUTS:
%    parameters  - A k+sum(archP)+sum(garchQ)+k*(k-1)/2 vector of parameters of the form
%                  [GarchParams(1) GarchParams(2) ... GarchParams(k) CCParams]
%                  where the garch parameters from each estimation are of the form
%                  [omega(i) alpha(i1) alpha(i2) ... alpha(ip(i)) beta(i1) beta(i2) ... beta(iq(i))]
%                  and CCparams are from ccvech of a correlation matrix
%    data        - A t by k matrix of zero mean residuals
%    archP       - A vector of arch innovation lag lengths
%    garchQ      - A vector of Garch AR lag lengths
% 
% OUTPUTS:
%    logL        - The estimated log likelihood
%    likelihoods - The likelihoods (t by 1)
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001



[t,k]=size(data);
%Seperate out the Correlation and the Garch process parameters
index=1;
H=zeros(t,k);
for i=1:k
    univariateparameters=parameters(index:index+archP(i)+garchQ(i));
    [simulatedata, H(:,i)] = dcc_univariate_simulate(univariateparameters,archP(i),garchQ(i),data(:,i));    
    index=index+archP(i)+garchQ(i)+1;
end
CorrParameters=parameters(index:length(parameters));
CorrMat=ccivech(CorrParameters,k);

%Ok, now we have everybody in the correct form, calculate the likeelihood

stdresid=data./sqrt(H);
Rinv=CorrMat^(-1);
Rdetlog=log(det(CorrMat));
logL=0;
likelihoods=zeros(t,1);
for j=1:t
   likelihoods(j)=k*log(2*pi)+sum(log(H(j,:)))+Rdetlog+stdresid(j,:)*Rinv*stdresid(j,:)';
   logL=logL+likelihoods(j);
end;
likelihoods=0.5*likelihoods;
logL=0.5*logL;









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Correlation=ccivech(parameters,numseries)
Correlation=diag(ones(numseries,1));
index=1;
for i=1:numseries
    for j=i+1:numseries
        Correlation(i,j)=parameters(index);
        Correlation(j,i)=Correlation(i,j);
        index=index+1;
    end
end





