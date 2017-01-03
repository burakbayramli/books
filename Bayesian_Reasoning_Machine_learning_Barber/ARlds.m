function [f g]=ARlds(v,L,sigma2V,sigma2H)
%ARLDS Learn AR coefficients using a Linear Dynamical System
% [f g]=ARlds(v,L,sigma2V,sigma2H)
% Learn AR coefficients using a Linear Dynamical System, where the
% coefficients are the latent variables and undergo Brownian motion
%
% Inputs:
% v : visble (observation) sequence
% L : order of the AR model
% sigma2V : output variance
% signma2H : transition variance on the coefficients
%
% Outputs:
% f : filtered p(a(t)|v(1:t))
% g : smoothed p(a(t)|v(1:T)
% See also demoARlds.m

% nb: this code does not exploit the sparse structure of A
A = eye(L);
CovH=sigma2H*eye(L); CovV=sigma2V;
meanH=zeros(L,1); meanV=0;
T=length(v);

f=zeros(L,T); F=zeros(L,L,T); logpvgv=zeros(1,T);
for t=L+1:T
	B = v(t-L:t-1);
	[f(:,t) F(:,:,t) logpvgv(t)]=LDSforwardUpdate(f(:,t-1),F(:,:,t-1),v(t),A,B,CovH,CovV,meanH,meanV);
end
g=zeros(L,T); G=zeros(L,L,T);
g(:,T)=f(:,T); G(:,:,T)=F(:,:,T);
for t=T-1:-1:1
	[g(:,t) G(:,:,t)]=LDSbackwardUpdate(g(:,t+1),G(:,:,t+1),f(:,t),F(:,:,t),A,CovH,meanH);
end