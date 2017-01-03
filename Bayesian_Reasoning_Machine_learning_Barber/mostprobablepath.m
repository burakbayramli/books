function [optpath logprob]=mostprobablepath(logtransition,startstate,endstate)
%MOSTPROBABLEPATH Find the most probable path in a Markov Chain
% [optpath logprob]=mostprobablepath(logtransition,startstate,endstate)
% Find the most probable path from the startstate to the endstate for a
% log transition probability matrix, transition=log(p).
%
% Inputs:
% logtransition : a logtransition matrix log(p(i,j)) from j->i
% startstate : the beginning state
% endstate : the desired end state
%
% Outputs:
% optpath: the most probable path from startstate to endstate
% logprob: The log probability of the path
%
% This can be used to solve shortest summed weighted path problems (no sign constraint) 
% by calling with logtransition = edgeweight provided no positive cycles exist)
% Can also solve the shortest path (including cycles) by calling with
% logtransition(i,j)=log(A) for adjacency matrix A(sink,source) (zero diagonal)
% see also mostprobablepathmult.m
N=size(logtransition,1);
E(1)=logtransition(endstate,startstate); % starting log path probability
lambda{1}=logtransition(:,startstate); % initial message
for t=2:N
	for s=1:N
		lambda{t}(s,1)= max(lambda{t-1}+logtransition(s,:)'); % message recursion
	end
	E(t)=lambda{t}(endstate); % optimal log path probability after t timesteps
end
[logprob topt]=max(E); % optimal log path probability and number of timesteps
% backtrack:
if isfinite(logprob)
	sopt(topt)=endstate; % optimal state at time topt
	for t=topt:-1:2
        sopt(t-1)=argmax(lambda{t-1} + logtransition(sopt(t),:)');
	end
	optpath=[startstate sopt];
else
	optpath=nan;
end