function value = EMvalueTable(R, P, state_size, action_size,cut_off, gamma)
%EMVALUETABLE MDP solver calculates the value function of the MDP with the current policy
% value = EMvalueTable(R, P, state_size, cut_off, gamma)
value = zeros(state_size, 1);
for s = 1:state_size
    state = zeros(state_size, 1);
    state(s,1) = 1;
    for t = 1:cut_off
        value(s,1) = value(s,1) + gamma^t*sum(R*state)/action_size;
        state = P*state;
    end
end