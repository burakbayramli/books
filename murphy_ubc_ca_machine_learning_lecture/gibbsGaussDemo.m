% gibbsGaussDemo
% Use Gibbs sampling to sample from a 2D Gaussian

S = 5000;
mu = [1 1];
C = [2 1; 1 1];

% try different starting seeds to check if mixing
seeds = [1 2 3];
figure; colors = {'r', 'g', 'b', 'k'};
for seedi=1:length(seeds)
  seed = seeds(seedi);
  rand('state', seed); randn('state', seed);
  xinit = 20*rand(2,1); % initial state
  samples = gibbsGauss(mu, C, xinit, S);
  burnin  = 1000;
  samples = samples(burnin+1:end,:);
  plot(samples(:,1), colors{seedi});
  hold on
end

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
