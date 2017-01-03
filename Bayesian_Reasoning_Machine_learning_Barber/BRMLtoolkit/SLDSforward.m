function [f F alpha w loglik]=SLDSforward(v,A,B,CovH,CovV,meanH,meanV,CovP,meanP,tranS,priorS,I)
%SLDSFORWARD Switching Latent Linear Dynamical System Gaussian Sum forward pass
%  [f F alpha w loglik]=SLDSforward(v,A,B,CovH,CovV,meanH,meanV,CovP,meanP,tranS,priorS,I)
%
% Inputs:
% v : observations
% A : transition matrix
% B : emission matrix
% CovH : transition covariance
% CovV : emission covariance
% meanH : transition mean
% meanV : emission mean
% covP : initial prior
% meanP : initial mean
% transS : switch transition distribution
% prior : switch initial distribution
% I : number of Gaussian components in the Gaussian Sum approximation
%
% Outputs:
% f : filterered mean p(h(t|v(1:t))
% F: filterered covariance p(h(t)|v(1:t))
% alpha: filtered switch distribution p(s(t)|v(1:t))
% w : mixture weights
% loglik: log likelihod of the sequence log p(v(1:T))
% See also SLDSbackward.m, demoSLDStraffic.m
S=size(A,3); T=size(v,2); H=size(A,1);
w=zeros(I,S,T); loglik=0;
f=zeros(H,I,S,T); F=zeros(H,H,I,S,T);
% first time-step (t=1)
for s=1:S
    [f(:,1,s,1),F(:,:,1,s,1),logphat]=...
        LDSforwardUpdate(zeros(H,1),zeros(H,H),v(:,1),A(:,:,s),B(:,:,s),CovP(:,:,s),CovV(:,:,s),meanP(:,s),meanV(:,s));
    logalpha(s,1) = sumlog(priorS(s))+logphat;
end
alpha(:,1)=condexp(logalpha);
w(1,:,1)=1;
% remaining time-steps:
for t=2:T
    if t==2;Itm=1; else Itm=I;end
    for st=1:S
        ind=0;
        for i=1:Itm
            for s=1:S
                ind=ind+1;
                [mu(:,ind),Sigma(:,:,ind),logphat]=LDSforwardUpdate(f(:,i,s,t-1),F(:,:,i,s,t-1),v(:,t), ...
                    A(:,:,st),B(:,:,st),CovH(:,:,st),CovV(:,:,st),meanH(:,st),meanV(:,st));
                logp(st,ind)=sumlog([w(i,s,t-1) tranS(st,s) alpha(s,t-1)])+logphat;
            end
        end
        % collapse:
        [w(:,st,t), f(:,:,st,t), F(:,:,:,st,t)] = mix2mix(condexp(logp(st,:)'), mu, Sigma, I);
        logalpha(st)=logsumexp(logp(st,:),ones(1,ind));
        %if mean(abs(f(:)))>100000; keyboard; end
    end
    alpha(:,t)=condexp(logalpha);
    loglik=loglik+logsumexp(logp(:),ones(S*ind,1));
end