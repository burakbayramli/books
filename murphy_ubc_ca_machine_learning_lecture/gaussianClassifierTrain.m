function params = fitGaussianClassifier(traindata)
% Fit class conditional Gaussians and multinomial priors
% Input:
% traindata.X is an n x p matrix
% traindata.C specifies the class label (1..K)
%
% Output:
% params.mu(d,c) for feature d, class c
% params.Sigma(:,:,c) covariance for class c
% params.sigma(d,c): std for feature d, class c
% params.classPrior(c)

Nclasses = length(unique(traindata.C));
n = length(traindata.C);

for c=1:Nclasses
  ndx = find(traindata.C == c);
  dat = traindata.X(ndx, :);
  params.mu(:,c) = mean(dat);
  params.Sigma(:,:,c) = cov(dat);
  params.sigma(:,c) = sqrt(diag(params.Sigma(:,:,c))); % diagonal
  params.classPrior(c) = length(ndx)/n;
end
% For tied parameters
dat = traindata.X;
params.SigmaPooled = cov(dat);
params.sigmaPooled = sqrt(diag(params.SigmaPooled));
