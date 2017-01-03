function logbeta=HMMbackward(v,phghm,pvgh)
%HMMBACKWARD HMM Backward Pass
% logbeta=HMMbackward(v,phghm,pvgh)
%
% Inputs:
% v : visible (observation) sequence being a vector v=[2 1 3 3 1 ...]
% phghm : homogeneous transition distribution phghm(i,j)=p(h(t)=i|h(t-1)=j)
% ph1 : initial distribution
% pvgh : homogeneous emission disrtribution pvgh(i,j)=p(v(t)=i|h(t)=j)
% 
% Outputs:
% logbeta: log beta messages: log p(v(t+1:T)|h(t))
% See also HMMbackward.m, HMMviterbi.m, demoHMMinference.m
T=length(v); H=size(phghm,1);
% logbeta recursion
logbeta(:,T)=zeros(H,1);
for t=T:-1:2
	logbeta(:,t-1)=logsumexp(repmat(logbeta(:,t),1,H),repmat(pvgh(v(t),:)',1,H).*phghm);
end

if 1==0 % beta recursion (not recommended due to numerical underflow)
	beta(:,T)=ones(H,1);
	for t=T:-1:2
		beta(:,t-1)=phghm'*(beta(:,t).*pvgh(v(t),:)');
	end
end