function samples = gibbsGauss(mu, Sigma, xinit, Nsamples)
% Gibbs sampling for a multivariate Gaussian
%
% Input:
% mu(1:D) is the mean
% Sigma(1:D, 1:D) is the covariance
% xinit(1:D) is the initial state
% Nsamples = number of samples to draw
%
% Output:
% samples(t,:)

D = length(mu);
samples = zeros(Nsamples, D);
x = xinit(:)';
for s=1:Nsamples
  for i=1:D
    [muAgivenB, sigmaAGivenB] = gaussCondition(mu, Sigma, i, x);
    x(i) = normrnd(muAgivenB, sqrt(sigmaAGivenB)); 
  end
  samples(s,:) = x;
end
