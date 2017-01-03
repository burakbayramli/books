function q = EMqUtilMarginal(initial_state, R, pi, P, cut_off_T, state_size, gamma)
%EMQUTILMARGINAL Returns term proportional to the q marginal for the utility term
q = zeros(state_size,1);
q_forward = initial_state;
q_backward = sum(R.*pi,1)';
for t = 1:cut_off_T
    if(sum(q_backward.*q_forward) ~= 0)
        q = q + gamma^t*q_backward.*q_forward/sum(q_backward.*q_forward);
    end
    q_forward = P*q_forward;
end