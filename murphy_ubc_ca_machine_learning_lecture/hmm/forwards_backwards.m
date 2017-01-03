function [gamma, loglik, xi_summed, samples] = forwards_backwards_v1(initDist, transmat, obslik, nsamples)
% INPUT:
% initDist(i) = Pr(Q(1) = i)
% transmat(i,j) = Pr(Q(t) = j | Q(t-1)=i)
% obslik(i,t) = Pr(Y(t)| Q(t)=i)  
% nsamples - num samples to draw (default 0)
%
% OUTPUT:
% gamma(i,t) = p(Q(t)=i | y(1:T))
% loglik = log p(y(1:T))
% xi_summed(i,j) = sum_{t=2}^{T} xi(i,j,t) 
%   where  xi(i,j,t)  = p(Q(t-1)=i, Q(t)=j | y(1:T))  
% samples(t,s) = value of Q(t)  in sample s


if nargin < 4, nsamples = 0; end

[Q T] = size(obslik);
loglik = 0;
alpha = zeros(Q,T);
gamma = zeros(Q,T);
xi_summed = zeros(Q,Q);
samples = zeros(T, nsamples);

%%%%%%%%% Forwards %%%%%%%%%%

t = 1;
alpha(:,1) = initDist(:) .* obslik(:,t);
[alpha(:,t), scale(t)] = normalize(alpha(:,t));
for t=2:T
  m = transmat' * alpha(:,t-1);
  alpha(:,t) = m(:) .* obslik(:,t);
  [alpha(:,t), scale(t)] = normalize(alpha(:,t));
end
loglik = sum(log(scale+eps));

%%%%%%%%% Backwards %%%%%%%%%%

beta = zeros(Q,T);
t=T;
beta(:,T) = ones(Q,1);
gamma(:,T) = normalize(alpha(:,T) .* beta(:,T));
if nsamples > 0
  samples(t,:) = sample(gamma(:,T), nsamples);
end
for t=T-1:-1:1
 b = beta(:,t+1) .* obslik(:,t+1);
 beta(:,t) = normalize(transmat * b);
 gamma(:,t) = normalize(alpha(:,t) .* beta(:,t));
 xi_summed = xi_summed + normalize((transmat .* (alpha(:,t) * b')));
 if nsamples > 0
   % compute xi_{t+1|t+1}(i,j)
   xi_filtered = normalize((alpha(:,t) * obslik(:,t+1)') .* transmat);
   for n=1:nsamples
     dist = normalize(xi_filtered(:,samples(t+1,n)));
     samples(t,n) = sample(dist);
   end
 end
end

