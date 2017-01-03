function [optpath logprob]=mostprobablepathmult(logtransition)
% mostprobablepathmult Find the all source all sink most probable paths in a Markov Chain
% [optpath logprob]=mostprobablepathpaths(logtransition)
% Find the most probable path from the startstate to the endstate for a
% log transition probability matrix, transition=log(p).
%
% Inputs:
% logtransition : a logtransition matrix log(p(i,j)) from j->i
%
% Outputs:
% optpath(startstate,endstate,t): contains the state at time t beginning at startstate and ending in endstate
% logprob(startstate,endstate): The log probability of the path
%
% This can be used to solve shortest summed weighted path problems (no sign constraint)
% by calling with transition = exp(edgeweight) (provided no positive cycles exist)
% use noselfpath(squeeze(optpath(startstate,endstate,:))') to find the path
% see also mostprobablepath.m and demoMostProbablePathMult.m
N=size(logtransition,1);
for i=1:N; if ~isnan(logtransition(i,i)); logtransition(i,i)=0; end; end % ensure zeros on diagonal when children exist
lambda{1}=logtransition; % initial message
for t=2:N
    for s=1:N
        for sp=1:N
            lambda{t}(s,sp)= max(lambda{t-1}(:,sp)+logtransition(s,:)'); % message recursion
        end
    end
end
logprob=lambda{N}';
% backtrack:
sopt(:,:,N)=repmat(1:N,N,1); % optimal state at time topt
for t=N:-1:2
    for s=1:N
        for sp=1:N
            sopt(s,sp,t-1) = argmax(lambda{t-1}(:,s)+logtransition(sopt(s,sp,t),:)'); 
        end
    end
end
optpath(:,:,1)=repmat((1:N)',1,N); optpath(:,:,2:N+1)=sopt;