function [g G gamma u]=SLDSbackward(v,f,F,rho,w,A,CovH,meanH,tranS,I,J,varargin)
%SLDSBACKWARD  Backward pass using a Mixture of Gaussians
% [g G gamma u]=SLDSbackward(v,f,F,rho,w,A,CovH,meanH,tranS,I,J,<1/0>)
% if optional argument=0 then do Generalised Pseudo Bayes, otherwise Expectation Correction
%
% Inputs:
% v : observations
% f : filterered mean p(h(t|v(1:t))
% F: filterered covariance p(h(t)|v(1:t))
% rho :  filtered switch distribution p(s|v(1:t)) (also called alpha)
% w : mixture weights
% A : transition matrix
% CovH : transition covariance
% meanH : transition mean
% transS : switch transition distribution
% I : number of Gaussian components in the Forward Gaussian Sum approximation
% J : number of Gaussian components in the Backward Gaussian Sum approximation
%
% Outputs:
% g : smoothed mean p(h(t)|v(1:T))
% G: filterered covariance p(h(t)|v(1:T))
% gamma: smoothed switch distribution p(s(t)|v(1:T))
% u : mixture weights
% See also SLDSforward.m, demoSLDStraffic.m
if isempty(varargin)
    doEC=1;
else
    doEC=varargin{1};
end

S=size(A,3); T=size(v,2); H=size(A,1);
u=zeros(J,S,T);
g=zeros(H,J,S,T); G=zeros(H,H,J,S,T);

gamma(:,T)=rho(:,T);
if J<I
    for st=1:S
        [u(:,st,T),g(:,:,st,T),G(:,:,:,st,T)]=mix2mix(w(:,st,T),f(:,:,st,T),F(:,:,:,st,T),J);
    end
else
    g(:,:,:,T)=f(:,:,:,T); G(:,:,:,T)=F(:,:,:,T); u(:,:,T)=w(:,:,T);
end

for t=T-1:-1:1
    if t==1;It=1; else It=I;end
    Jtp=J;
    
    for st=1:S
        for it=1:It
            for stp=1:S
                pststpgV1t(it,st,stp)=tranS(stp,st)*w(it,st,t)*rho(st,t);
                Stp = A(:,:,stp)*F(:,:,it,st,t)*A(:,:,stp)'+CovH(:,:,stp);
                logdet2piStp = logdet(2*pi*Stp);
                tmpvec=A(:,:,stp)*f(:,it,st,t)+meanH(:,stp);
                for jtp=1:Jtp
                    [mu(:,it,st,jtp,stp),Sigma(:,:,it,st,jtp,stp)]=LDSbackwardUpdate(g(:,jtp,stp,t+1),G(:,:,jtp,stp,t+1),...
                        f(:,it,st,t),F(:,:,it,st,t),A(:,:,stp),CovH(:,:,stp),meanH(:,stp));
                    if doEC
                        % compute contribution to mixture weight:
                        % ztp= <htp|stp,jtp,v1:T>-<htp|st,stp,it,v_{1:t}>; Stp is the covariance
                        ztp = g(:,jtp,stp,t+1) - tmpvec;
                        tmp1=-0.5*ztp(:)'*(Stp\ztp(:))-0.5*logdet2piStp;
                        logtmp2(it,st,jtp,stp)=log(pststpgV1t(it,st,stp)+eps) +tmp1; % Expectation Correction
                    else
                        logtmp2(it,st,jtp,stp)=log(pststpgV1t(it,st,stp)+eps); % Generalised Pseudo Bayes
                    end
                end
            end
        end
    end
    
    pitstgjtpstpV1T=reshape(condexp(reshape(logtmp2,I*H,I*H)),I,H,I,H); % p(st,it|stp,jtp,v1:T)=p(st,it,stp,jtp|v1:T)/p(stp,jtp,v1:T)
    
    for st=1:S
        ind=0;
        gamma(st,t)=0;
        for it=1:It
            for stp=1:S
                for jtp=1:Jtp
                    % p(st,stp,it,jtp|v1:T)=p(stp|V1:T)*p(jtp|stp,v1:T)p(st,it|stp,jtp,V1:T)
                    pmixjoint(it,st,jtp,stp)=gamma(stp,t+1)*u(jtp,stp,t+1)*pitstgjtpstpV1T(it,st,jtp,stp);
                    gamma(st,t)=gamma(st,t) + pmixjoint(it,st,jtp,stp);
                    ind=ind+1;
                    muComp(:,ind)=mu(:,it,st,jtp,stp);
                    SigmaComp(:,:,ind)=Sigma(:,:,it,st,jtp,stp);
                    pComp(ind,1)= pmixjoint(it,st,jtp,stp);
                end
            end
        end
        pComp=condp(pComp);
        [u(:,st,t),g(:,:,st,t),G(:,:,:,st,t)]=mix2mix(pComp,muComp,SigmaComp,J);  % Project to a mixture
    end
    
end