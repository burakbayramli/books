function [logalpha,loglik]=HMMforward(v,phghm,ph1,pvgh)
%HMMFORWARD HMM Forward Pass
% [logalpha,loglik]=HMMforward(v,phghm,ph1,pvgh)
%
% Inputs:
% v : visible (observation) sequence being a vector v=[2 1 3 3 1 ...]
% phghm : homogeneous transition distribution phghm(i,j)=p(h(t)=i|h(t-1)=j)
% ph1 : initial distribution
% pvgh : homogeneous emission disrtribution pvgh(i,j)=p(v(t)=i|h(t)=j)
% 
% Outputs:
% logalpha : log alpha messages: log p(h(t),v(1:t))
% loglik : sequence log likelihood log p(v(1:T))
% See also HMMbackward.m, HMMviterbi.m, demoHMMinference.m
T=length(v); H=length(ph1);
% logalpha recursion
logalpha(:,1) = log(pvgh(v(1),:)'.*ph1);
for t=2:T
	logalpha(:,t)=logsumexp(repmat(logalpha(:,t-1),1,H),repmat(pvgh(v(t),:),H,1).*phghm');
end
loglik = logsumexp(logalpha(:,T),ones(H,1)); % log likelihood

if 1==0 % alpha recursion (not recommended due to numerical underflow)
	alpha(:,1) = pvgh(v(1),:)'.*ph1;
	for t=2:T
		alpha(:,t)=pvgh(v(t),:)'.*(phghm*alpha(:,t-1));
	end
	loglik = log(sum(alpha(:,T))); % log likelihood
end