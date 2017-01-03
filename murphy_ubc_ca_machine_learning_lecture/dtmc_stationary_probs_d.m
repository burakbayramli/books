function pi_vec = dtmc_stationary_probs_d(trans_mat)
% function pi_vec = dtmc_stationary_probs_d(trans_mat)
% Stationary probability of a discrete time markov chain
% Andrew M. Ross http://www.lehigh.edu/~amr5/q/matlab.html
% Use the drop-one-equation method.

ImP = eye(size(trans_mat)) - trans_mat ;
rhs = zeros(1,size(trans_mat,2)) ; % as many columns as the transition matrix

% now, replace the last column with zeros
ImP(:,end) = 0;
% except fix pi_vec(1) at 1
ImP(1,end) = 1;
% and its right-hand-size has a one as well
rhs(end) = 1;

% solve the equation pi_vec * ImP = rhs
pi_vec = rhs / ImP;

% normalize the pi_vec
pi_vec = pi_vec / sum(pi_vec);



