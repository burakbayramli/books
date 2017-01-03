function mhDemoMOG()
% Demo of Metropolis-Hastings algorithm for sampling from 
% a mixture of two 1D Gaussians using a Gaussian proposal.
% Based on code originally written by Nando de Freitas.

weights = [0.3 0.7];
mus = [0 10];
sigmas = [2 2];

Nsamples = 5000;       
x = zeros(Nsamples,1); 
sigma_prop = 10;                 % Standard deviation of the Gaussian proposal.

targetArgs = {weights, mus, sigmas};
proposalArgs = {sigma_prop};

seed = 1; randn('state', seed); rand('state', seed);
xinit = 20*rand(1,1); % initial state
[x, naccept] = MH(@target, @proposal, xinit, Nsamples,  targetArgs, proposalArgs);

% Let us check the asymmetric proposal works
%seed = 1; randn('state', seed); rand('state', seed);
%xinit = 20*rand(1,1); % initial state
%[x2, naccept] = MH(@target, @proposal, xinit, Nsamples,  targetArgs, proposalArgs, @proposalProb);
%assert(approxeq(x, x2))

% plot the histogram of samples
N_bins = 50; 
Ns = [100 500 1000 Nsamples];
figure;
for i=1:4
  subplot(2,2,i)
  x_t = linspace(-10,20,1000);
  y_t = feval(@target, x_t, weights, mus, sigmas);
  [b,a] = hist(x(1:Ns(i)), N_bins);
  measure = a(2)-a(1); % bin width.
  area = sum(b*measure);
  bar(a,b/(area),'y')
  hold on;
  plot(x_t,y_t,'k','linewidth',2)
  axis([-10 20 0 .15])
  text(14,.1,sprintf('N=%d', Ns(i)))
end

%%%%%%%%%%

function p = mogProb(x, mixWeights, mu, sigma)

% p(n) = sum_k w(k) N(x(n)|mu(k), sigma(k))
K = length(mixWeights);
N = length(x);
p = zeros(N,1);
for k=1:K
  p = p + mixWeights(k)*mvnpdf(x(:), mu(k), sigma(k));
end

function  p = target(x, mixWeights, mus, sigmas)
p = log(mogProb(x, mixWeights, mus, sigmas));

function xp = proposal(x, sigma_prop)
xp = x + sigma_prop*randn(1,1);

function p = proposalProb(x, xprime, sigma_prop)
p = normpdf(x, xprime, sigma_prop);

