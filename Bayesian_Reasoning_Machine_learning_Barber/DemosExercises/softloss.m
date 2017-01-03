function l = softloss(x,c,w,b,lambda,beta)
%SOFTLOSS Soft loss function
% l = softloss(x,c,w,b,lambda,beta)
% see also demoSoftLoss.m
N=size(x,2);
l= sum((sigmoid(w'*x+repmat(b,1,N),beta)-c).^2)+lambda*w(:)'*w(:);

%keyboard