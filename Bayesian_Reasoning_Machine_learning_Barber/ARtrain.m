function [a residual]=ARtrain(v,L)
%ARTRAIN Fit autoregressive (AR) coefficients of order L to v.
% [a residual]=ARtrain(v,L)
%
% Inputs:
% v: input sequence
% L : order of the AR model
% 
% Outputs:
% a : learned AR coefficients
% residual : error in the prediction of each point

% This uses a simple Gaussian Elimination solver
% -- Levinson Durbin is a recommended alternative
vvhat=zeros(L,1); v=v(:);
vhatvhat=zeros(L,L);
for t=L+1:length(v)
	vhat = v(t-L:t-1);
	vvhat = vvhat + v(t)*vhat;
	vhatvhat = vhatvhat + vhat*vhat';
end
a = (vhatvhat+eps*eye(size(vhatvhat)))\vvhat;
for t=L+1:length(v(:))
	vhat = v(t-L:t-1);
	residual(t)=v(t)-a'*vhat;
end

