function gamma=HMMgamma(logalpha,phghm)
%HMMGAMMA HMM Posterior smoothing using the Rauch-Tung-Striebel correction method
% gamma=HMMbackward(logalpha,phghm)
%
% Inputs:
% logalpha : log alpha forward messages (see HMMforward.m)
% phghm : transition distribution in a matrix
% 
% Outputs: gamma(i,t) is p(h(t)=i|v(1:T))
% See also HMMbackward.m, HMMviterbi.m, demoHMMinference.m
T=size(logalpha,2); H=size(phghm,1);
% gamma recursion using logalpha
gamma(:,T)=condexp(logalpha(:,T));
for t=T-1:-1:1
	phghp = condp(phghm'.*repmat(condexp(logalpha(:,t)),1,H));
	gamma(:,t) = condp(phghp*gamma(:,t+1));
end

if 1==0 % gamma recursion: More human readable
	alpha=exp(logalpha);
	gamma(:,T)=alpha(:,T)./sum(alpha(:,T));
	for t=T-1:-1:1
		phghp = phghm'.*repmat(alpha(:,t),1,H);
		phghp = phghp./repmat(sum(phghp,1),H,1);
		gamma(:,t) = phghp*gamma(:,t+1);
	end
end