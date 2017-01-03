function r = mvrandn(mu,sigma,varargin)
%MVRANDN Samples from a multi-variate Normal(Gaussian) distribution
% r = mvrandn(mu,sigma,<cases>)
% draw <cases> samples from a Gaussian with mean vector mu and covariance sigma
if isempty(varargin)
    cases=1;
else
    cases=varargin{1};
end
D=length(mu); % dimension of space
[U S V]= svd(sigma);
r=randn(D,cases);
r = U*diag(sqrt(diag(S)))*r + repmat(mu(:),1,cases);