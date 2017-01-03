function SAdemoMOG()
% Demo of Simulated Annealing for finding the mode of
% a mixture of two 1D Gaussians using a Gaussian proposal.

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

initTemp = 1;
coolingFactor = 0.995;
for t=1:Nsamples
  if t==1
    temp(1) = initTemp;
  else
    temp(t) = temp(t-1) * coolingFactor; % cool down
  end
end
%figure; plot(temp)
%temp([100 500 1000 5000])

[x, naccept] = SA(@target, @proposal, xinit, Nsamples,  targetArgs, proposalArgs, ...
		  [], temp);

% plot the histogram of samples
N_bins = 50; 
Ns = [100 500 1000 Nsamples];
figure;
for i=1:4
  subplot(2,2,i)
  x_t = linspace(-10,20,1000);
  y_t = exp(feval(@target, x_t, weights, mus, sigmas));
  yy_t = y_t .^ (1/temp(Ns(i)));
  [b,a] = hist(x(1:Ns(i)), N_bins);
  measure = a(2)-a(1); % bin width.
  area = sum(b*measure);
  bar(a,b/(area),'y')
  hold on;
  plot(x_t,y_t,'k','linewidth',2)
  axis([-10 20 0 .15])
  text(14,.1,sprintf('N=%d', Ns(i)))
end

figure;
for i=1:4
  subplot(2,2,i)
  x_t = linspace(-10,20,1000);
  y_t = exp(feval(@target, x_t, weights, mus, sigmas));
  yy_t = y_t .^ (1/temp(Ns(i)));
  plot(x_t, yy_t, 'k', 'linewidth', 2);
  title(sprintf('temp = %4.4f', temp(Ns(i))))
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
p = mogProb(x, mixWeights, mus, sigmas);
p = log(p+eps);

function xp = proposal(x, sigma_prop)
xp = x + sigma_prop*randn(1,1);

function p = proposalProb(x, xprime, sigma_prop)
p = normpdf(x, xprime, sigma_prop);

