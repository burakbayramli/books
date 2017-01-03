function [weights, principalcomponets, eigenvalues,explainedvariance, cumR2]=pca(data,type)
% PURPOSE:
%      To perform Centered(demeaned) Principal Componet Analysis for use in the O_MVGARCH proceedure
% 
% 
% USAGE:
%      [weights, principalcomponets, eigenvalues,explainedvariance, cumR2]=pca(data,type);
% 
% 
% INPUTS:
%      data    - a t by k matrix to be decomposed using PCA
%      type    - (optional) a string, wither 'cov' or 'corr' that denotees the way
%                you wish to use PCA, on either the Covariance Matrix or Correlation MAtrix
%                both are useful and the cov is similar to TMW princomp
% 
% OUTPUTS:
%      weights            - a k by k matrix of componet weights
%      principalcomponets - a t by k matrix of principal componets
%      eigenvalues        - The eeigenvaluse associated with each PC
%      explainedvariance  - The percent of the viariance explained by each PC
%      cumR2              - The cumulative R2 of including the ith PC
% 
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
% 


t=size(data,1);
data=data-repmat(mean(data),t,1);
if nargin==1 || strcmp(type,'Corr')
    newdata=data./repmat(std(data),t,1);
    CorrMat=corrcoef(newdata);
else
    newdata=data;
    CorrMat=cov(data);
end

[EigenVects,EigenVals]=eig(CorrMat);
EigenVals=fliplr(flipud(EigenVals));
EigenVects=fliplr(flipud(EigenVects));
tempdata=fliplr(newdata);
weights=EigenVects;

principalcomponets=tempdata*EigenVects;
weights=flipud(weights);
eigenvalues=diag(EigenVals);
explainedvariance=eigenvalues/sum(eigenvalues);
cumR2=cumsum(explainedvariance);

