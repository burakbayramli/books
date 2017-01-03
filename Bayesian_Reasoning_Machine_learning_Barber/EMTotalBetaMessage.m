function beta_total = EMTotalBetaMessage(cut_off_T, R, T, pi, state_size, action_size)
%EMTOTALBETAMESSAGE backward information needed to solve the MDP process using message passing
beta = R;
beta_total = zeros(action_size, state_size, cut_off_T+1);
beta_total(:, :, 1) = beta;
for t = 1:cut_off_T
    beta =  reshape(sum(beta.*pi,1)*T, state_size, action_size)';
    beta_total(:, :, t+1) = beta;
end