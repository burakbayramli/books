% dishonest casino example 2 in matlab

data = [1 2 4 5 5 2 6 4 6 2 1 4 6 1 4 6 1 3 6 1 3 6 6 6 1 6 6 4 6 6 1 6 3 6 6 1 6 3 6 6 1 6 3 6 1 6 5 1 5 6 1 5 1 1 5 1 4 6 1 2 3 5 6 2 3 4 4];

O = 6; Q = 2;

% generate prior random matrices
prior0 = normalise(rand(Q,1));
transmat0 = mk_stochastic(rand(Q,Q));
obsmat0 = mk_stochastic(rand(Q,O));

[LL, prior1, transmat1, obsmat1] = dhmm_em(data, prior0, transmat0, obsmat0, 'max_iter', 10);

prior1
transmat1
obsmat1

% generate prior random matrices
prior0 = normalise(rand(Q,1));
transmat0 = mk_stochastic(rand(Q,Q));
obsmat0 = mk_stochastic(rand(Q,O));

[LL, prior1, transmat1, obsmat1] = dhmm_em(data, prior0, transmat0, obsmat0, 'max_iter', 10);

prior1
transmat1
obsmat1
