function [f,F,L]=LDSforward(v,A,B,CovH,CovV,meanH,meanV,CovP,meanP)
%LDSFORWARD Full Forward Pass for a Latent Linear Dynamical System (Kalman Filter)
% [f,F,L]=LDSforward(v,A,B,CovH,CovV,meanH,meanV,CovP,meanP)
%
% Forward pass of an LDS for a H dimensional hidden variable and V
% dimensional visible (observation) variable 
%
% Inputs:
% v : observation matrix of dimension V x T. Each column contains a datapoint, v(:,1:T)
% A : H x H transition matrix
% B : V x H emission matrix
% CovH : H x H transition covariance
% CovV : V x V emission covariance
% meanH : H x 1 transition mean
% meanV : V x 1 emission mean
% covP : H x H initial prior
% meanP : H x 1 initial mean
%
% Outputs:
% f : filterered mean p(h(t|v(1:t))
% F: filterered covariance p(h(t)|v(1:t))
% L: log likelihod of the sequence log p(v(1:T))
[V H]=size(B); T=size(v,2);
f=zeros(H,T); F=zeros(H,H,T); logpvgv=zeros(1,T);

[f(:,1) F(:,:,1) logpvgv(1)]=LDSforwardUpdate(zeros(H,1),zeros(H,H),v(:,1),A,B,CovP,CovV,meanP,meanV);
for t=2:T
	[f(:,t) F(:,:,t) logpvgv(t)]=LDSforwardUpdate(f(:,t-1),F(:,:,t-1),v(:,t),A,B,CovH,CovV,meanH,meanV);
end
L=sum(logpvgv);