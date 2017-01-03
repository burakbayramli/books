function [g,G]=LDSbackward(v,A,B,f,F,CovH,meanH)
%LDSBACKWARD Full Backward Pass for a Latent Linear Dynamical System (RTS correction method)
% [g,G]=LDSbackward(v,A,B,f,F,CovH,meanH) 
%
% Inputs:
% v : observation v(t+1)
% A : transition matrix
% B : emission matrix
% f : forward pass means
% F : forward pass covariances
% CovH : transition covariance
% CovV : emission covariance
% meanH : transition mean
%
% Outputs:
% g : smoothed mean p(h(t|v(1:t))
% F: smoothed covariance p(h(t)|v(1:t))
[V H]=size(B); T=size(v,2);
g=zeros(H,T); G=zeros(H,H,T);
g(:,T)=f(:,T); G(:,:,T)=F(:,:,T);
for t=T-1:-1:1
    [g(:,t) G(:,:,t)]=LDSbackwardUpdate(g(:,t+1),G(:,:,t+1),f(:,t),F(:,:,t),A,CovH,meanH);
end