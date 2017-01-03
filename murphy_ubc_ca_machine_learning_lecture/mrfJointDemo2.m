% mrfJointDemo2

% run mrfJointDemo first!

% find the MAP state 
[junk, xMAP] = max(joint) % 16
xMAPbits = ind2subv(2*ones(1,4), xMAP) % 2,2,2,2

% sample from the distribution (with replacement)
S = 1000;
K = length(joint);
%samples = randsample(1:K, S, true, joint)
samples = sample_discrete(joint, 1, S);
h = hist(samples,1:K);
bar(normalize(h))

% compute cond14(x4, x1) = p(x1|x4)
J14 = marginalizePot(J, [1 4]);
cond14 = mk_stochastic(J14.T'); % satisfies sum_x1 cond14(x4,x1) = 1
sum(cond14,2) % column of 1s
