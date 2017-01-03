function pi_vec = dtmc_stationary_probs_o(trans_mat)
% function pi_vec = dtmc_stationary_probs_o(trans_mat)
% Stationary probability of a discrete time markov chain
% Andrew M. Ross http://www.lehigh.edu/~amr5/q/matlab.html
% Use the matrix-of-ones method suggested by Resnick, page 138


ImPpO = eye(size(trans_mat)) - trans_mat + ones(size(trans_mat));
rhs = ones(1,size(trans_mat,2)) ; % as many columns as the transition matrix

% solve the equation pi_vec * ImPpO = rhs
pi_vec = rhs / ImP;


