function pi_vec = dtmc_stationary_probs_e(trans_mat)
% function pi_vec = dtmc_stationary_probs_e(trans_mat)
% Stationary probability of a discrete time markov chain
% Andrew M. Ross http://www.lehigh.edu/~amr5/q/matlab.html
% Use the eigenvector method.

[e_vecs, e_vals] = eig(trans_mat'); % note the transpose!
tmp = e_vecs(:, 1); % assume first evec corresponds to evalue 1
% the eigenvector doesn't always add up to 1, so re-normalize.
% Also, convert to a row vector
pi_vec = tmp' ./ sum(tmp);



