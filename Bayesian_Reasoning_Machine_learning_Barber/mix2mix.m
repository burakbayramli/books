function [newcoeff, newmean, newcov] = mix2mix(coeff, mean, cov, I)
%MIX2MIX Fit a mixture of Gaussians with another mixture of Gaussians
% (but with a smaller number of components I) by retaining the
% I-1 most probable coeffs, and merging the rest.
%
% Inputs:
% coeff(:) : coefficients of the mixtures
% mean(:,coeff) : the corresponding means
% cov(:,:,coeff) : corresponding covariance matrices
% I : desired number of smaller mixtures
%
% Outputs:
% newcoeff(:) : new mixture coefficients
% newmean(:,:) : new means
% newcov(:,:,:) : new covariance matrices
% See also SLDSforward.m, SLDSbackward.m
newcoeff = coeff; newmean  = mean; newcov   = cov;
L = length(newcoeff);
if L > I
    [val,ind] = sort(coeff);
    tomerge   = ind(1:L-I+1);
    notmerged = setdiff(1:L,tomerge);
    sump = sum(coeff(tomerge));
    if sump ~= 0
        condp = coeff(tomerge) ./ sump;
    else
        n     = length(tomerge);
        condp = ones(1,n) / n;
    end
    [mergedmean, mergedcov] = matmixtosingle(condp, mean(:,tomerge), cov(:,:,tomerge));
    newcoeff(tomerge) = [];  newmean(:,tomerge)  = [];  newcov(:,:,tomerge) = [];
    newcoeff(I) = sump;  newmean(:,I)  = mergedmean;  newcov(:,:,I) = mergedcov;
end

function [m, S] = matmixtosingle(coeff, means, cov)
% fit a mixture of Gaussians with a single Gaussian, so that the first and second moments
% of the fitted Gaussian match the mixture first and second moments.
n=size(means,2); m=zeros(size(means,1),1); S=zeros(size(means,1));
for i=1:n
    m=m+coeff(i)*means(:,i); S=S+coeff(i)*(cov(:,:,i)+means(:,i)*means(:,i)');
end
S=S-m*m';