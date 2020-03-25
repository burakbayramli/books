%CE_example_gibbs.m
n=5;gamma=1.99;
N=10^5;      % length of Markov chain
u=ones(1,n); % starting value for Gibbs sampling
v=0;
% run the Gibbs sampler
for i=1:N
    I=ceil(rand*n);u(I)=0;
    lower=max(gamma-h(u),0);
    u(I)=lower+(1-lower)*rand;
    v=log(u)+v;  % compute the mean of the log(u)
end
% compute the Maximum Likelihood estimate
v=-N./v;

% apply Importance sampling
N1=10^5;
nu = repmat(v,N1,1);
U = rand(N1,5).^(1./nu);
I =h(U)>gamma;
w=zeros(size(I)); % ensure w=0 where h(U)<gamma
w(I) = prod(1./(nu(I,:).*U(I,:).^(nu(I,:) - 1)),2);
% deliver the final estimator
est = mean(w)
percRE = std(w)/sqrt(N1)/est*100

