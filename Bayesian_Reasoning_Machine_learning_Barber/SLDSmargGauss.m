function [meanGauss, covGauss]=SLDSmargGauss(weights,ps,means,covs)
%SLDSMARGGAUSS compute the single Gaussian from a weighted SLDS mixture
% [meanGauss, covGauss]=SLDSmargGauss(weights,ps,means,covs)
[S T]=size(ps);
J=size(means,2);
for t=1:T;	ind=0;
    for s=1:S
        for j=1:J
            ind=ind+1; coeff(ind)=weights(j,s,t)*ps(s,t);
            mn(:,ind) = means(:,j,s,t); cv(:,:,ind)=covs(:,:,j,s,t);
        end
        [dum,meanGauss(:,t),covGauss(:,:,t)]=mix2mix(coeff,mn,cv,1); % find the mean of the Gaussian mixture
    end
end
