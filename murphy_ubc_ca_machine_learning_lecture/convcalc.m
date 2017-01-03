function [EPSR, W, B, V] = convcalc(chains)

% chains is m by n
% 2*n is the total length of each chain, where we
% take the first n samples as burnin and the n+1 to 2n samples for analysis

[m, n] = size(chains);

% Estimate B
psibar_i = mean(chains, 2);        % Mean of each chain
B = n*cov(psibar_i);

% Estimate W
s = chains - repmat(psibar_i, 1, n);
stwo_i = sum(s.*s, 2)/(n-1);
W = sum(stwo_i)/m;

% estimate target variance
V = ((W.*(n-1))./n)+(B./n);
R = V/W;
EPSR = R.^0.5;
