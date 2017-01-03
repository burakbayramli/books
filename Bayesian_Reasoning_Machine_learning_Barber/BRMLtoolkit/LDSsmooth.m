function [f,F,g,G,Gp,loglik]=LDSsmooth(v,A,B,CovH,CovV,CovP,meanP,meanH,meanV)
%LDSSMOOTH Linear Dynamical System : Filtering and Smoothing
%[f,F,g,G,Gp]=LDSsmooth(v,A,B,CovH,CovV,CovP,meanP,meanH,meanV)
% 
% Inputs:
% v : V x T matrix of observations
% A : transition matrix
% B : emission matrix
% CovH : transition covariance
% CovV : emission covariance
% CovP : initial (prior) covariance
% meanP : initial (prior) mean
% meanH : mean of the hidden transition
% meanV : mean of the observation
%
% Outputs:
% f,F : filtered mean and covariance
% g,G : smoothed mean and covariance
% Gp : smoothed cross moment <h_t h_{t+1}|v_{1:T}> t=1..T-1
% loglik : log likelihood of the sequence v
H=size(A,1); V=size(CovV,1); T = size(v,2); Id = eye(H);
v=v-repmat(meanV,1,T);

% Forward Pass
K = CovP*B'*inv(B*CovP*B'+CovV);
F{1} = (Id - K*B)*CovP;
f{1} = (Id - K*B)*meanP+K*v(:,1);
mu=B*meanP; 
S = B*CovP*B'+CovV;
del = v(:,1)-mu; 

loglik = del'*(S\del) + log(det(S))+V*log(2*pi); % log likelihood initialisation
%loglik = del'*(S\del) + logdet(2*pi*S); % log likelihood initialisation

for t=2:T
	P = A*F{t-1}*A' + CovH;
	K = P*B'/(B*P*B'+CovV);
%    K = P*B'*inv(B*P*B'+CovV);
	F{t} = (Id - K*B)*P;
	tmp = A*f{t-1}+meanH;
	f{t} = tmp + K*(v(:,t)-B*tmp);
	mu=B*A*f{t-1};
    S = B*(A*F{t-1}*A'+CovH)*B'+CovV;
	del = v(:,t)-mu; 
    %loglik = loglik + del'*(S\del) + logdet(2*pi*S); % log likelihood 
    loglik = loglik + del'*(S\del) + log(det(S))+V*log(2*pi); % log likelihood 
end
loglik = -0.5*loglik;
% Backward Pass (Gamma RTS correction smoother)
G{T} = F{T}; g{T} = f{T};
for t=T-1:-1:1
	leftA = F{t}*A'*inv(A*F{t}*A' + CovH);
	G{t} = F{t}+ leftA*(G{t+1}-A*F{t}*A'-CovH)*leftA';
	g{t} = f{t}+leftA*(g{t+1} -A*f{t}-meanH);
	Gp{t} = leftA*G{t+1}+g{t}*g{t+1}'; % smoothed correlation <h_t h_{t+1}>
end

