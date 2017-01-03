% from http://hh.diva-portal.org/smash/get/diva2:291586/FULLTEXT01.pdf

nex=12;
data=[1416.6,1418.34,1409.71,1412.84,1412.11,1414.85,1423.82,1430.73,1431.9,1430.62,1426.37,1430.5];
O = 1;
%%M=1;
M=2;
Q=3;
prior0=normalise(rand(Q,1));
transmat0 = mk_stochastic(rand(Q,Q));
transmat0
[mu0, Sigma0] = mixgauss_init(Q*M, data, 'full');
mu0 = reshape(mu0,[O Q M])
Sigma0 = reshape(Sigma0, [O O Q M])
mixmat0 = mk_stochastic(rand(Q,M));
[LL, prior1, transmat1, mu1, Sigma1,mixmat1] = ...
    mhmm_em(data, prior0,transmat0, mu0, Sigma0, mixmat0, 'max_iter', 10)

%% transmat0 =

%%     0.4235    0.1291    0.4474
%%     0.4730    0.4091    0.1179
%%     0.0482    0.4727    0.4792


%% mu0(:,:,1) =

%%    1.0e+03 *

%%     1.4133    1.4306    1.4319


%% mu0(:,:,2) =

%%    1.0e+03 *

%%     1.4251    1.4097    1.4175

%% iteration 1, loglik = -35.709761
%% iteration 2, loglik = -25.306294
%% iteration 3, loglik = -22.138367
%% iteration 4, loglik = -20.562739
%% iteration 5, loglik = -20.023233
%% iteration 6, loglik = -20.023233

%% LL =

%%   -35.7098  -25.3063  -22.1384  -20.5627  -20.0232  -20.0232


%% prior1 =

%%     0.0000
%%          0
%%     1.0000


%% transmat1 =

%%     0.6000    0.4000    0.0000
%%     0.6667    0.0000    0.3333
%%     0.0000    0.6667    0.3333


%% mu1(:,:,1) =

%%    1.0e+03 *

%%     1.4133    1.4306    1.4319


%% mu1(:,:,2) =

%%    1.0e+03 *

%%     1.4251    1.4097    1.4175


%% Sigma1(:,:,1,1) =

%%     1.3523


%% Sigma1(:,:,2,1) =

%%     0.0188


%% Sigma1(:,:,3,1) =

%%     0.0100


%% Sigma1(:,:,1,2) =

%%     1.6359


%% Sigma1(:,:,2,2) =

%%     0.0100


%% Sigma1(:,:,3,2) =

%%     0.7669


%% mixmat1 =

%%     0.6000    0.4000
%%     0.7500    0.2500
%%     0.3333    0.6667
