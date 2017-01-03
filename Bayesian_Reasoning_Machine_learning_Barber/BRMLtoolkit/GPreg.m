function [meantest,vartest,logpygx]=GPreg(xtrain,ytrain,xtest,par,covfn,varytrain)
%GPREG Gaussian Process Regression
% [meantest,vartest,logpygx]=GPreg(xtrain,ytrain,xtest,par,covfn,varytrain)
%
% Inputs:
% xtrain : matrix of training data. Each column contains a datapoint
% ytrain : vector of 1 dimensional outputs corresponding to xtrain
% xtest  : the test inputs
% par : covariance parameters
% covfn : covariance function
% varytrain: training output variance
% 
% Outputs:
% meantest: mean of the clean underlying function on the test points
% vartest : variance of the clean underlying function on the test points
% logpygx: log likelihood of the training data 
%
% See demoGPreg.m
N=size(xtrain,2);
if isscalar(varytrain)
    varytrain=varytrain*ones(1,N);
end
K = feval(covfn,xtrain,xtrain,par)+diag(varytrain); % Covariance function for noisy outputs
for t=1:size(xtest,2)
	xs=xtest(:,t);
	Kxs=feval(covfn,xtrain,xs,par(1:3));
	Kss=feval(covfn,xs,xs,par(1:3));
	tmp = K\Kxs;
	meantest(:,t) = ytrain(:)'*tmp;
	vartest(:,t) = Kss - tmp'*Kxs;
end
logpygx = -0.5*ytrain(:)'*K/ytrain(:)'-0.5*logdet(K)-0.5*N*log(2*pi); % training data log likelihood
