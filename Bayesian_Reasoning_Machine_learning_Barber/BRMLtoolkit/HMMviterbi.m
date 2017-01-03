function [maxstate logprob]=HMMviterbi(v,phghm,ph1,pvgh)
%HMMVITERBI Viterbi most likely joint hidden state of a HMM
% [maxstate logprob]=HMMviterbi(v,phghm,ph1,pvgh)
%
% Inputs:
% v : visible (obervation) sequence being a vector v=[2 1 3 3 1 ...]
% phghm : homogeneous transition distribution phghm(i,j)=p(h(t)=i|h(t-1)=j)
% ph1 : initial distribution
% pvgh : homogeneous emission disrtribution pvgh(i,j)=p(v(t)=i|h(t)=j)
%
% Outputs:
% maxstate : most likely joint hidden (latent) state sequence
% logprob : associated log probability of the most likely hidden sequence
% See also demoHMMinference.m
T=size(v,2); H=size(phghm,1);
mu(:,T)=ones(H,1);
for t=T:-1:2
	tmp = repmat(pvgh(v(t),:)'.*mu(:,t),1,H).*phghm;
	mu(:,t-1)= max(tmp)';
	mu=condp(mu); % normalise to avoid underflow
end
% backtrack
[val hs(1)]=max(ph1.*pvgh(v(1),:)'.*mu(:,1));
for t=2:T
	tmp = pvgh(v(t),:)'.*phghm(:,hs(t-1));
	[val hs(t)]=max(tmp.*mu(:,t));
end
maxstate=hs;
logprob=log(ph1(hs(1)))+log(pvgh(v(1),hs(1)));
for t=2:T
	logprob=logprob+log(phghm(hs(t),hs(t-1)))+log(pvgh(v(t),hs(t)));
end