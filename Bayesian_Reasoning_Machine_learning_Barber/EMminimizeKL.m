function best_actions = EMminimizeKL(q_tran, q_util, T, R, action_size)
%EMMINIMIZEKL MDP deterministic policy solver. Finds optimal actions
% best_actions = EMminimizeKL(q_tran, q_util, T, R, action_size)
best_score = -10e100;
best_actions = [];
T(T == 0) = 1e-16; % to avoid log 0
R(R == 0) = 1e-16; % to avoid log 0
for a = 1:action_size
    if(sum(q_tran.*log(T(:,a))) + q_util*log(R(a)) > best_score)
        best_score = sum(q_tran.*log(T(:,a))) + q_util.*log(R(a));
        best_actions = a;
    elseif(sum(q_tran.*log(T(:,a))) + q_util*log(R(a)) == best_score)
        best_actions = [best_actions a];
    end
end