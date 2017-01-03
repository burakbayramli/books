function mhDemoGauss2d()
% Demo of Metropolis-Hastings algorithm for sampling from 
% a 2D Gaussian using a Gaussian proposal.
% Compare to gibbsGaussDemo.m

Nsamples = 5000;       
burnin  = 1000;

%sigma = 0.01; % does't mix
sigma = 1; % mixes
SigmaProp = sigma*eye(2);

mu = [0 0];
C = [2 1; 1 1];

targetArgs = {mu, C};
proposalArgs = {SigmaProp};

% try different starting seeds to check if mixing
seeds = [1 2 3];
figure; colors = {'r', 'g', 'b', 'k'};
samples = zeros(Nsamples-burnin, 2, length(seeds));
for c=1:length(seeds)
  seed = seeds(c);
  randn('state', seed); rand('state', seed);
  xinit = 20*rand(2,1); % initial state
  [tmp, naccept] = MH(@target, @proposal, xinit, Nsamples,  targetArgs, proposalArgs);
  samples(:,:,c) = tmp(burnin+1:end,:);
  plot(samples(:,1,c), colors{c});
  hold on
end
Rhat1 = EPSR(squeeze(samples(:,1,:)))
Rhat2 = EPSR(squeeze(samples(:,2,:)))
title(sprintf('sigmaProposal = %3.2f, Rhat=%5.3f', sigma, Rhat1))


figure;
h=draw_ellipse(mu', C);
set(h, 'linewidth', 3, 'color', 'r');
axis equal
set(gca, 'xlim', [-5 5]);
set(gca, 'ylim', [-5 5]);
hold on
ndx = 1:10:size(samples,1); % only plot subset of points
plot(samples(ndx,1), samples(ndx,2), 'k.');

% Plot 1D exact and approximate marginals
for i=1:2
  figure;
  Nbins = 100;
  [h, xs] = hist(samples(:,1),Nbins);
  binWidth = xs(2)-xs(1);
  bar(xs, normalise(h)/binWidth);
  hold on
  ps = normpdf(xs, mu(i), sqrt(C(i,i)));
  plot(xs, ps, '-');
  title(sprintf('x%d', i))
end

%%%%%%%%%%

function  p = target(x, mu, Sigma)
p = log(mvnpdf(x(:)', mu, Sigma));

function xp = proposal(x, SigmaProp)
xp = mvnrnd(x, SigmaProp);


