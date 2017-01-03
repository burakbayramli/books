function [a,sigma2,stran,phtgV1T]=SARlearn(v,L,S,Tskip,opts)
%SARLEARN EM training of a Switching AR model
% [a,sigma2,stran,phtgV1T]=SARlearn(v,L,S,opts)
%
% Inputs:
% v :  a single timeseries is contained in the row vector v
% L :  order of tha AR model 
% S : number of AR models.
% Tskip forces the switches to make a transition only at times t for mod(t,Tskip)==0
% opts.maxit
% opts.plotprogress
% 
% Outputs:
% a : learned AR coefficients
% sigma2 : learned innovation noise
% stran : learned transition distribution
% phtgV1T : smoothed posterior p(h(t)|v(1:T))
% See also demoSARinference.m
a=condp(randn(L,S)); % set the AR coefficients
stran=condp(ones(S,S)); % switch transition
sprior=condp(ones(S,1)); % switch prior
sigma2=var(v)*ones(1,S);
T=size(v,2);

for emloop=1:opts.maxit
    % Inference using HMM structure:
    [logalpha,loglik]=HMMforwardSAR(v,stran,sprior,a,sigma2,Tskip);
    logbeta=HMMbackwardSAR(v,stran,a,sigma2,Tskip);
    [phtgV1T,phthtpgV1T]=HMMsmoothSAR(logalpha,logbeta,a,sigma2,stran,v,Tskip);

    loglikem(emloop)=loglik;
    if opts.plotprogress; plot(loglikem); title('log likelihood'); drawnow; end
    for s=1:S
        vvhat_sum=zeros(L,1); vhatvhat_sum=zeros(L,L); sigma_sum=0; sigma_num=0;
        for t=1:T
            Lt = min(t-1,L); % to handle the start when not enough timepoints
            vhat=zeros(L,1);
            vhat(end-Lt+1:end) = v(t-Lt:t-1)';
            m = a(:,s)'*vhat; % means
            vvhat_sum = vvhat_sum + phtgV1T(s,t)*v(t)*vhat./sigma2(s);
            vhatvhat_sum = vhatvhat_sum + phtgV1T(s,t)*vhat*vhat'./sigma2(s);
            sigma_sum = sigma_sum+phtgV1T(s,t)*(v(t)-m).^2;
            sigma_num = sigma_num + phtgV1T(s,t);
        end
        a(:,s)=vhatvhat_sum\vvhat_sum;
        sigma2(s)=sigma_sum/sigma_num;
	end
    t=1:T-1; tt=t(find(mod(t+1,Tskip)==0));
    stran=condp(sum(phthtpgV1T(:,:,tt),3)');
end