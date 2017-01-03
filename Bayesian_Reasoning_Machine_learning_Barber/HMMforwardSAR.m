function [logalpha,loglik]=HMMforwardSAR(v,phghm,ph1,a,sigma2,Tskip)
%HMMFORWARDSAR Switching Autoregressive HMM with switches updated only every Tskip timesteps
% [logalpha,loglik]=HMMforwardSAR(v,phghm,ph1,a,sigma2,Tskip)
% 
% Inputs:
% v : observations
% phghm : state (switch) transition matrix p(h(t)|h(t-1))
% ph1 : prior state distribution
% a : matrix of AR coefficients. Column a(:,i) are the AR coeffs for switch state i. 
% (note that the AR coefficients are in reverse order)
% sigma2 : the innovation noise
% Tskip : the number of timesteps to skip before a switch update is allowed
%
% Outputs:
% logalpha : log forward messages
% loglik : sequence log likelihood log p(v(1:T))
% See also HMMbackwardSAR.m and demoSARlearn.m
T=length(v); [L H]=size(a);
% logalpha recursion:
logalpha(:,1) = -0.5*repmat(v(1).^2,H,1)./sigma2(:)- 0.5*log(2*pi*sigma2(:)) + log(ph1);
for t=2:T
	Lt = min(t-1,L); % to handle the start when not enough timepoints
	vhat = v(t-Lt:t-1)';
	m = a(L-Lt+1:L,:)'*vhat; % means
	d = repmat(v(t),H,1)-m;
	phatvgh=exp(-0.5*d.^2./sigma2(:))./sqrt(2*pi*sigma2(:))+eps;
	if mod(t,Tskip)==0 % only make a transition every Tskip timesteps
		phghmt=phghm;
	else
		phghmt=eye(H);
	end
	logalpha(:,t)=logsumexp(repmat(logalpha(:,t-1),1,H),repmat(phatvgh',H,1).*phghmt');
end
loglik = logsumexp(logalpha(:,T),ones(H,1)); % log likelihood