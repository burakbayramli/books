function [post,lik,lli] = softmaxApply(beta,x,y)
% [post,lik,lli] = softmaxApply(beta,x,y)
% Evaluate multinomial logistic regression model.
% Written by David Martin
% 
% INPUT
% 	beta 	dxk model coefficients (as returned by logistK)
% 	x 	dxn matrix of n input column vectors
% 	[y] 	kxn vector of class assignments
%
% OUTPUT
% 	post 	kxn fitted class posteriors
% 	lik 	1xn vector of sample likelihoods
%	lli	log likelihood
%
% Let p(i,j) = exp(beta(:,j)'*x(:,i)),
% Class j posterior for observation i is:
%	post(j,i) = p(i,j) / (p(i,1) + ... p(i,k))
% The likelihood of observation i given soft class assignments
% y(:,i) is: 
%	lik(i) = prod(post(:,i).^y(:,i))
% The log-likelihood of the model given the labeled samples is:
%	lli = sum(log(lik))
% 

% check sizes
if size(beta,1) ~= size(x,1),
  error('Inputs beta,x not the same height.');
end
if nargin > 3 & size(y,2) ~= size(x,2), 
  error('Inputs x,y not the same length.'); 
end

% get sizes
[d,k] = size(beta);
[d,n] = size(x);

% class posteriors
post = zeros(k,n);
bx = zeros(k,n);
for j = 1:k, 
  bx(j,:) = beta(:,j)'*x; 
end
for j = 1:k, 
  post(j,:) = 1 ./ sum(exp(bx - repmat(bx(j,:),k,1)),1);
end
clear bx;

% likelihood of each sample
if nargout > 1,
  y = y ./ repmat(sum(y,1),k,1); % L1-normalize class assignments
  lik = prod(post.^y,1);
end

% total log likelihood
if nargout > 2,
  lli = sum(log(lik+eps));
end;

% eof
