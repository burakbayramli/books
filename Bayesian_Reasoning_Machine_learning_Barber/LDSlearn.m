function [A B CovH CovV priormean CovP meanV meanH loglik]=LDSlearn(v,H,opts)
%LDSLEARN Learn parameters for a Latent Linear Dynamical System (Kalman Filter) 
% [A B CovH CovV priormean CovP meanV meanH loglik]=LDSlearn(v,H,opts)
%
% The method is based on maximum likelihood and the Expectation Maximisation algorithm
%
% Inputs:
% v : observation matrix of dimension V x T. Each column contains a datapoint, v(:,1:T)
% H : hidden dimension of the latent LDS
% opts.maxits : maximum number of iterations of the EM algorithm
% opts.diagCovV : set to 1 for a diagonal emission matrix
% opts.plotprogress : set to 1 to plot the log likelihood evolution
% opts.tol : log likelihood change termination criterion
% opts.init : set to 1 to use initialisation:
% opts.Ainit
% opts.Binit
% opts.CovVinit
% opts.CovHinit
% opts.CovPinit
% opts.priormeaninit
% opts.meanHinit
% opts.meanVinit
%
% Outputs:
% A : H x H transition matrix
% B : V x H emission matrix
% CovH : H x H transition covariance
% CovV : V x V emission covariance
% meanH : H x 1 transition mean
% meanV : V x 1 emission mean
% covP : H x H initial prior
% meanP : H x 1 initial mean
% loglik: log likelihod of the sequence log p(v)

loglikold=-realmax;
V=size(v,1); T=size(v,2);
if ~opts.init
    A=eye(H);
    [usvd ssvd vsvd]=svd(v);
    B=usvd(:,1:H);
    CovH=eye(H);
    CovP=eye(H);
    vv=usvd(:,1:H)*ssvd(1:H,1:H)*vsvd(1:H,:);
    CovV=cov((v-vv)');
    priormean=zeros(H,1);
    meanH=zeros(H,1); 
    meanV=zeros(V,1);
else
    A=opts.Ainit;
    B=opts.Binit;
    CovV=opts.CovVinit;
    CovH=opts.CovHinit;
    CovP=opts.CovPinit;
    meanH=opts.meanHinit;
    meanV=opts.meanVinit;
    priormean=opts.priormeaninit;
end

for loop=1:opts.maxits
    
    if opts.diagCovV       
        [f,F,g,G,Gp,lik]=LDSsmooth(v,A,B,CovH,diag(CovV),CovP,priormean,meanH,meanV);        
    else
        [f,F,g,G,Gp,lik]=LDSsmooth(v,A,B,CovH,CovV,CovP,priormean,meanH,meanV);
    end
       
    loglik(loop)=lik;
    if opts.plotprogress; plot(loglik,'-o');  drawnow; end
    if loop>1 & loglik(loop)<loglik(loop-1); warning('log likelihood decreased'); end
    if loglik(loop) - loglikold < opts.tol; break; end
    
    priormean=g{1};
    CovP=G{1}+g{1}*g{1}'-g{1}*priormean'-priormean*g{1}'+priormean*priormean';
    CovP=(CovP+CovP')/2;
    
    HH=zeros(H);
    HHp=zeros(H);
    VH=zeros(V,H);
    for t=1:T-1
        HH=HH+G{t}+g{t}*g{t}';
        HHp=HHp+Gp{t}';
        VH=VH+v(:,t)*g{t}';
    end
    A=HHp/HH;
    HH=HH+G{T}+g{T}*g{T}';
    VH=VH+v(:,T)*g{T}';
    B=VH/HH;
    CovV=zeros(V);
    meanV=zeros(V,1);
    for t=1:T
        CovV=CovV+v(:,t)*v(:,t)'-v(:,t)*g{t}'*B'-B*g{t}*v(:,t)'+B*(G{t}+g{t}*g{t}')*B';
        meanV=meanV+v(:,t)-B*g{t};
    end
    meanV=meanV/T;
    CovV=CovV/T;CovV=(CovV+CovV')/2;
    if opts.diagCovV; CovV=diag(CovV); end
    
    CovH=zeros(H); meanH=zeros(H,1);
    for t=1:T-1
        CovH=CovH+G{t+1}+g{t+1}*g{t+1}'-A*Gp{t}-Gp{t}'*A'+A*(G{t}+g{t}*g{t}')*A';
        meanH=meanH+g{t+1}-A*g{t};
    end
    CovH=CovH/(T-1); CovH=(CovH+CovH')/2;
    meanH=meanH/(T-1);
    loglikold=loglik;
end
loglik=lik;