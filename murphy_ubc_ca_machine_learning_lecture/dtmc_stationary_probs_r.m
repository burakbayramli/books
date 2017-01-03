function pi_vec = dtmc_stationary_probs_r(trans_mat)
% function pi_vec = dtmc_stationary_probs_r(trans_mat)
% Stationary probability of a discrete time markov chain
% Andrew M. Ross http://www.lehigh.edu/~amr5/q/matlab.html
% Use the replace-one-equation method.


ImP = eye(size(trans_mat)) - trans_mat ;
rhs = zeros(1,size(trans_mat,2)) ; % as many columns as the transition matrix

% now, replace the last column with ones
ImP(:,end) = 1;
% and its right-hand-size has a one as well
rhs(end) = 1;

% solve the equation pi_vec * ImP = rhs
pi_vec = rhs / ImP;



