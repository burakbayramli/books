function logbeta=HMMbackwardSAR(v,phghm,a,sigma2,Tskip)
%HMMBACKWARDSAR Backward Pass (beta method) for the Switching Autoregressive HMM
% logbeta=HMMbackwardSAR(v,phghm,a,sigma2,Tskip)
%
% Inputs:
% v : observations
% phghm : state (switch) transition matrix p(h(t)|h(t-1))
% a : matrix of AR coefficients. Column a(:,i) are the AR coeffs for switch state i. 
% (note that the AR coefficients are in reverse order)
% sigma2 : the innovation noise
% Tskip : the number of timesteps to skip before a switch update is allowed
%
% Outputs:
% logbeta: log backward messages log p(v(t+1:T)|h(t),v(t-L+1:t))
% See also HMMforwardSAR.m and demoSARlearn.m
T=length(v); [L H]=size(a);
% logbeta recursion
logbeta(:,T)=zeros(H,1);
for t=T:-1:2
    Lt = min(t-1,L); % to handle the start when not enough timepoints
    vhat = v(t-Lt:t-1)';
    m = a(L-Lt+1:L,:)'*vhat; % means
    d = repmat(v(t),H,1)-m;
    phatvgh=exp(-0.5*d.^2./sigma2(:))./sqrt(2*pi*sigma2(:))+eps;
    if mod(t,Tskip)==0
        phghmt=phghm;
    else
        phghmt=eye(H);
    end
    logbeta(:,t-1)=logsumexp(repmat(logbeta(:,t),1,H),repmat(phatvgh,1,H).*phghmt);
end