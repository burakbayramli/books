function [phtgV1T,phthtpgV1T]=HMMsmooth(logalpha,logbeta,pvgh,phghm,v)
%HMMSMOOTH Smoothing for a Hidden Markov Model (HMM)
% [phtgV1T,phthtpgV1T]=HMMsmooth(logalpha,logbeta,pvgh,phghm,v)
% Return the HMM pointwise p(h(t)|v(1:T)) and pairwise posterior p(h(t),h(t+1)|v(1:T)). 
%
% Inputs:
% logalpha : log alpha messages log p(h(t)|v(1:t))
% logbeta : log beta messgages log p(v(t+1:T)|h(t),v(t-L+1:t))
% pvgh : emission distribution p(v|h)
% phghm : transition distribution
% v : visible (observation) sequence
%
% Outputs:
% phtgV1T : smoothed posterior p(h(t)|v(1:T))
% phthtpgV1T : smoothed pair p(h(t),h(t+1)|v(1:T))
T=length(v); H=size(phghm,1);
% smoothed posteriors: pointwise marginals:
for t=1:T
	logphtgV1T(:,t)=logalpha(:,t)+logbeta(:,t); % alpha-beta approach
	phtgV1T(:,t)=condexp(logphtgV1T(:,t));
end
% smoothed posteriors: pairwise marginals p(h(t),h(t+1)|v(1:T)):
for t=1:T-1
	atmp=condexp(logalpha(:,t));
	btmp=condexp(logbeta(:,t+1));
	ctmp = repmat(atmp,1,H).*phghm'.*repmat(pvgh(v(t+1),:).*btmp',H,1);
	phthtpgV1T(:,:,t)=ctmp./sum(sum(ctmp));
end