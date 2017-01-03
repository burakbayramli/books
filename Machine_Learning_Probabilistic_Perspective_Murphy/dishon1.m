% dishonest casino example 1 in matlab
prior_casino=[0.5;0.5];
transmat_casino=[0.95 0.05;0.05 0.95];
obsmat_casino=[1/6 1/6 1/6 1/6 1/6 1/6;1/10 1/10 1/10 1/10 1/10 1/2];
data = [1 2 4 5 5 2 6 4 6 2 1 4 6 1 4 6 1 3 6 1 3 6 6 6 1 6 6 4 6 6 1 6 3 6 6 1 6 3 6 6 1 6 3 6 1 6 5 1 5 6 1 5 1 1 5 1 4 6 1 2 3 5 6 2 3 4 4];
local=multinomial_prob(data,  obsmat_casino);
path = viterbi_path(prior_casino, transmat_casino, local)

%     1     1     1     1     1     1     2     2     2     2     2     2     2
%     2     2     2     2     2     2     2     2     2     2     2     2     2
%     2     2     2     2     2     2     2     2     2     2     2     2     2
%     2     2     2     2     2     2     2     1     1     1     1     1     1
%     1     1     1     1     1     1     1     1     1     1     1     1     1
%     1     1
