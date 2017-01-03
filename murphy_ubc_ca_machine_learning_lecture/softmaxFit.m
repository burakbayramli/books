function [beta] = softmaxFit(y,x,w)
% softmaxFit MLE for multinomial logistic regression
% Based on code by David Martin 
%
% k = number of classes
% n = number of samples
% d = dimensionality of samples
%
% INPUT
% 	x 	dxn matrix of n input column vectors
% 	y 	kxn vector of class assignments
% 	[w]	optional 1xn vector of sample weights 
%
% OUTPUT
% 	beta 	dxk matrix of fitted model coefficients 
%
% Let p(i,j) = exp(beta(:,j)'*x(:,i)),
% Class j posterior for observation i is:
%	post(j,i) = p(i,j) / (p(i,1) + ... p(i,k))
%
% See also softmaxApply.

[d,nx] = size(x);
[k,ny] = size(y);

% check sizes
if k < 2,
  error('Input y must encode at least 2 classes.');
end
if nx ~= ny,
  error('Inputs x,y not the same length.'); 
end

n = nx;

% make sure class assignments have unit L1-norm
sumy = sum(y,1);
if abs(1-sumy) > eps,
  sumy = sum(y,1);
  for i = 1:k, y(i,:) = y(i,:) ./ sumy; end
end
clear sumy;

beta = 1e-3*rand(d,k);
beta(:,k) = 0;	% fix beta for class k at zero

w = ones(1,n);
% normalize sample weights so max is 1
w = w / max(w);

options = optimset('Display','none','Diagnostics','off','GradObj','on','Hessian','on');
[beta, err] = fminunc(@softmaxGradient, beta, options, x, y, w);

%%%%%%%%%%%

function [f, g, H] = softmaxGradient(beta, X, y, w)

post = computePost(beta, X);
f = computeLogLik(post, y, w);
[g,H] = derivs(post,X,y,w);
f = -f; % negative log likelihood (minimization)
g = -g;

%%%%%%%%%%%

function post = computePost(beta,x)
  [d,n] = size(x);
  [d,k] = size(beta);
  post = zeros(k,n);
  bx = zeros(k,n);
  for j = 1:k, 
    bx(j,:) = beta(:,j)'*x; 
  end
  for j = 1:k, 
    post(j,:) = 1 ./ sum(exp(bx - repmat(bx(j,:),k,1)),1);
  end
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% log likelihood
function lli = computeLogLik(post,y,w)
  [k,n] = size(post);
  lli = 0;
  for j = 1:k,
    lli = lli + sum(w.*y(j,:).*log(post(j,:)+eps));
  end
  if isnan(lli), 
    error('lli is nan'); 
  end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gradient and hessian
%% These are computed in what seems a verbose manner, but it is
%% done this way to use minimal memory.  x should be transposed
%% to make it faster.
function [g,h] = derivs(post,x,y,w)

  [k,n] = size(post);
  [d,n] = size(x);

  % first derivative of likelihood w.r.t. beta
  g = zeros(d,k-1);
  for j = 1:k-1,
    wyp = w .* (y(j,:) - post(j,:));
    for ii = 1:d, 
      g(ii,j) = x(ii,:) * wyp'; 
    end
  end
  g = reshape(g,d*(k-1),1);

  % hessian of likelihood w.r.t. beta
  h = zeros(d*(k-1),d*(k-1)); 
  for i = 1:k-1,	% diagonal
    wt = w .* post(i,:) .* (1 - post(i,:));
    hii = zeros(d,d);
    for a = 1:d,
      wxa = wt .* x(a,:);
      for b = a:d,
        hii_ab = wxa * x(b,:)';
        hii(a,b) = hii_ab;
        hii(b,a) = hii_ab;
      end
    end
    h( (i-1)*d+1 : i*d , (i-1)*d+1 : i*d ) = -hii;
  end
  for i = 1:k-1,	% off-diagonal
    for j = i+1:k-1,
      wt = w .* post(j,:) .* post(i,:);
      hij = zeros(d,d);
      for a = 1:d,
        wxa = wt .* x(a,:);
        for b = a:d,
          hij_ab = wxa * x(b,:)';
          hij(a,b) = hij_ab;
          hij(b,a) = hij_ab;
        end
      end
      h( (i-1)*d+1 : i*d , (j-1)*d+1 : j*d ) = hij;
      h( (j-1)*d+1 : j*d , (i-1)*d+1 : i*d ) = hij;
    end
  end
