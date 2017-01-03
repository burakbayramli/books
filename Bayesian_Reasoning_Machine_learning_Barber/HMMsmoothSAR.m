function [phtgV1T,phthtpgV1T]=HMMsmoothSAR(logalpha,logbeta,a,sigma2,phghm,v,Tskip)
%HMMSMOOTHSAR Switching Autoregressive HMM smoothing
% [phtgV1T,phthtpgV1T]=HMMsmoothSAR(logalpha,logbeta,a,sigma2,phghm,v,Tskip)
% return the smoothed pointwise posterior p(h(t)|v(1:T)) and pairwise smoothed posterior p(h(t),h(t+1)|v(1:T)). 
%
% Inputs:
% logalpha : log alpha messages (see HMMforwardSAR.m)
% logbeta : log beta messages (see HMMbackwardSAR.m)
% a : matrix of AR coefficients. Column a(:,i) are the AR coeffs for switch state i. 
% (note that the AR coefficients are in reverse order)
% sigma2 : the innovation noise
% phghm : state (switch) transition matrix p(h(t)|h(t-1))
% v : observations
% Tskip : the number of timesteps to skip before a switch update is allowed
%
% Outputs:
% phtgV1T : smoothed posterior p(h(t)|v(1:T))
% phthtpgV1T  : smoothed posterior p(h(t),h(t+1)|v(1:T))
% See also HMMforwardSAR.m, HMMbackwardSAR.m, demoSARlearn.m
T=length(v); [L H]=size(a);
% smoothed posteriors: pointwise marginals:
for t=1:T
	logphtgV1T(:,t)=logalpha(:,t)+logbeta(:,t); % alpha-beta approach
	phtgV1T(:,t)=condexp(logphtgV1T(:,t));
end
% smoothed posteriors: pairwise marginals p(h(t),h(t+1)|v(1:T)):
for t=2:T
	atmp=condexp(logalpha(:,t-1));
	btmp=condexp(logbeta(:,t));
	Lt = min(t-1,L); % to handle the start when not enough timepoints
	vhat = v(t-Lt:t-1)';
	m = a(L-Lt+1:L,:)'*vhat; % means
	d = repmat(v(t),H,1)-m;
	logphatvgh=-0.5*d.^2./sigma2(:)-0.5*log(2*pi*sigma2(:));
	phatvgh=condexp(logphatvgh);
	if mod(t,Tskip)==0
		phghmt=phghm;
	else
		phghmt=eye(H);
	end
	ctmp = repmat(atmp,1,H).*phghmt'.*repmat(phatvgh'.*btmp',H,1)+eps; % two timestep potential
	phthtpgV1T(:,:,t-1)=ctmp./sum(sum(ctmp));
end