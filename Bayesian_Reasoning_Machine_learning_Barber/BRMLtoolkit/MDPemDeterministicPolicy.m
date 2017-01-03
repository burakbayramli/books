function [value pi]= MDPemDeterministicPolicy(transition, reward, cut_off, state_size, action_size, gamma,opts)
%MDPEMDETERMINISTICPOLICY Solve MDP using EM with deterministic policy
%[value pi]= MDPemDeterministicPolicy(transition, reward, cut_off, state_size, action_size, gamma,opts)
% reward(action,state)>=0  describes the rewards
% transition(i,j,a) = p(x(t)=i|x(t-1)=j,d(t-1)=a)
% cut_off is the finite time horizon
% opts.maxiterations
% opts.plotprogress
% opts.tol
%
% see demoMDP.m
A = zeros(state_size^2, state_size);
for i = 1:state_size
    A((1:state_size) + (i-1)*state_size, i) = 1;
end
T = reshape(transition, state_size, state_size*action_size);
pi = action_size^-1*ones(action_size, state_size); % uniform initial policy
initial_state = 0.01*ones(state_size,1);
oldvalue=zeros(state_size,1);
for i = 1:opts.maxiterations
    P = reshape(T, state_size^2, action_size)*pi;
    P = reshape(P(A == 1), state_size, state_size);
    q_tran = EMqTranMarginal(T, reward, pi, initial_state, P, cut_off, state_size, action_size, gamma);
    q_util = EMqUtilMarginal(initial_state, reward, pi, P, cut_off, state_size, gamma);
    for s = 1:state_size
        pi(:,s) = 0;
        best_actions = EMminimizeKL(q_tran(:,s), q_util(s), squeeze(transition(:,s,:)), reward(:,s), action_size);
        action = best_actions(1,randgen(1:size(best_actions,2))); % randomly select if more than one optimal
        pi(action, s) = 1; % deterministic policy
    end
    
    P = reshape(T, state_size^2, action_size)*pi;
    P = reshape(P(A == 1), state_size, state_size);
    value = EMvalueTable(reward, P, state_size, action_size, cut_off, gamma);
    value = value/gamma; % divide by gamma for comparison with standard methods
    
    if opts.plotprogress; bar(value(:)); title(['EM iteration ',num2str(i)]); drawnow; end
    if mean(abs(value-oldvalue))<opts.tol; break; end % stop if converged
    oldvalue=value;
end