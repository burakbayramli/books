function mhDemoBino()
% Demo of MH for sampling from a binomial with a non-conjugate prior

Nsamples = 40000;
burnin = 2000;
%sigmaProp = 0.5; % mixes well
sigmaProp = 10; % does not mix well
targetArgs = {};
proposalArgs = {sigmaProp};

seed = 1;
randn('state', seed); rand('state', seed);
xinit = rand(1,1); % initial state
[samples, naccept] = MH(@target, @proposal, xinit, Nsamples,  targetArgs, proposalArgs);
samples = samples(burnin+1:end,:);

% convert back to 0..1 scale
origSamples = (0.5+exp(samples))  ./ (1+exp(samples));

folder = 'C:\kmurphy\figures\other';
Nbins = 50;
figure;
hist(samples, Nbins)
title(['p(\phi|D), \sigma=' sprintf('%3.2f', sigmaProp)])
print(gcf, '-depsc', fullfile(folder, sprintf('mhDemoBino_phi_%3.2f', sigmaProp)))
figure;
hist(origSamples, Nbins)
title(['p(\theta|D), \sigma=' sprintf('%3.2f', sigmaProp)])
print(gcf, '-depsc', fullfile(folder, sprintf('mhDemoBino_theta_%3.2f', sigmaProp)))
figure;
plot(samples(end-500:end,1))
title(['\sigma=' sprintf('%3.2f', sigmaProp)])
print(gcf, '-depsc', fullfile(folder, sprintf('mhDemoBino_mix_%3.2f', sigmaProp)))


function xprime = proposal(x, sigmaProp)
xprime = normrnd(x, sigmaProp);

function logp = target(x)
ep = exp(x);
p = ((0.5+ep)^12 * ep)/((1+ep)^22);
logp = log(p+eps);
