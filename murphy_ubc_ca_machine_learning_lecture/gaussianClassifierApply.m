function post = gaussianClassifierApply(Xtest, params, d);
% gaussianClassifierApply - apply Bayes rule with Gaussian class-conditioanl densities.
%
% post = gaussianClassifierApply(Xtest, params);
% Computes post(i,c) = P(C=c|x(i,:)) using full untied covariance matrices
%
% post = gaussianClassifierApply(Xtest, params, d);
% If d is specified, we just use x(i,d), mu(d,:) and Sigma(d,d,:)
% ie. we just use feature d.

if nargin < 3, d = []; end

Ntest = size(Xtest,1);
Nclasses = length(params.classPrior);
lik = zeros(Ntest, Nclasses);
for c=1:Nclasses
  % lik(i, c) = p(x(i,:), mu(:, c), Sigma(:,:, c)
  if ~isempty(d) % scalar case
    lik(:,c) = mvnpdf(Xtest(:,d), params.mu(d, c)', params.Sigma(d,d, c));
  else
    lik(:,c) = mvnpdf(Xtest, params.mu(:, c)', params.Sigma(:,:, c));
  end
  % for naive Bayes, use this line  
  %lik(:,c) = mvnpdf(Xtest, params.mu(:, c)', diag(params.sigma(:,c)));
end
classPrior = params.classPrior;
%postNaive = normalizeRows(lik .* repmat(classPrior, Ntest, 1));
% To avoid underflow...
N = size(Xtest,1);
logjoint = log(lik) + repmat(log(classPrior(:)'), N, 1);
logpost = logjoint - repmat(logsumexp(logjoint,2), 1, Nclasses);
post = exp(logpost);
%assert(approxeq(post, postNaive))

