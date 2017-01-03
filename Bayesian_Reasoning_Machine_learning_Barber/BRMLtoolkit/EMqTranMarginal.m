function q_tran = EMqTranMarginal(T, R, pi, initial_state, P, cut_off, state_size, action_size, gamma)
%EMQTRANMARGINAL EM marginal transition in MDP
% q_tran = EMqTranMarginal(T, R, pi, initial_state, P, cut_off, state_size, action_size, gamma)
% q_tran proportional to q(new_state|old_state) in EM learning of determinitic MDP policy
T_pi = reshape(sum(reshape(repmat(reshape(pi',1,state_size*action_size),state_size,1).*T, ...
    state_size^2, action_size),2), state_size, state_size);
q_tran = zeros(state_size, state_size);
alpha = initial_state;
beta_total = EMTotalBetaMessage(cut_off, R, T, pi, state_size, action_size);
for tau = 1:cut_off-1
    q_tau = zeros(state_size, state_size);
    for t = tau+1:cut_off
        beta = sum(beta_total(:,:,t-tau),1)';
        q_tau = q_tau + gamma^t*T_pi.*(beta*alpha');
    end
    if(sum(sum(q_tau)) ~= 0)
        q_tran = q_tran + q_tau/sum(sum(q_tau));
    end
    alpha = P*alpha;
end