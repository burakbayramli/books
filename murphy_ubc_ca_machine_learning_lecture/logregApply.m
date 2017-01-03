function p = logregApply(X, beta)
% p(i) = prob(y=1|X(i,:)) = sigma(beta^T X(i,:))
p = 1./(1+exp(-X*beta(:)));
