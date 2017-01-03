function mh_demo()

seed = 1; randn('state', seed); rand('state', seed);

% This demo shows how the Metropolis-Hastings algorithm can be used to
% sample from a mixture of 2 Gaussians if we can sample from a Gaussian proposal. 
% Based on code originally written by Nando de Freitas.
% Modified by Kevin Murphy.

N = 5000;                        % Number of iterations (samples).
sigma = 2;                       % Standard deviation of the target components.
x = zeros(N,1);                  % Markov chain (unknowns).
sigma_prop = 10;                 % Standard deviation of the Gaussian proposal.

weights = [0.3 0.7];
mus = [0 10];
sigmas = [sigma sigma];

% do MCMC

x(1) = 20*rand(1,1);
for i=2:N
  u = rand(1,1);
  z = sigma_prop * randn(1,1);
  xprop = x(i-1) + z;
  alpha1 = mogProb(xprop, weights, mus, sigmas);
  alpha2 = mogProb(x(i-1), weights, mus, sigmas);
  alpha = min(1, alpha1/alpha2);
  if(u<alpha)
    x(i) = xprop; 
  else
    x(i) = x(i-1);
  end
end

% plot the histogram of samples

N_bins = 50;                     % Number of bins in the histogram.
Ns = [100 500 1000 N];
figure;
for i=1:4
  subplot(2,2,i)
  x_t = linspace(-10,20,1000);
  y_t = mogProb(x_t, weights, mus, sigmas);
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
  %p = p + mixWeights(k)*gaussian_prob(x(:)', mu(k), sigma(k)^2);
  p = p + mixWeights(k)*mvnpdf(x(:), mu(k), sigma(k));
end
