function demoGPclass1D
% DEMOGPCLASS1D: Gaussian Process Binary Classification using Laplace with fixed hyperparameters

figure
% make some training data
M=20;
xtrain=randn(1,M); % class 1
xtrain=[xtrain (randn(1,M)+5*ones(1,M))]; % class 0
xtrain=[xtrain (randn(1,M)-5*ones(1,M))]; % class 0
ctrain(M+1:3*M)=0; ctrain(1:M)=1; % training class labels
N=3*M; % total number of training points

% define the Kernel matrix:
par(1).value=2; % prefactor
par(2).value=1; % inverse root lengthscale
par(3).value=1; % gamma exponential parameters
par(4).value=0.0001; % self variance term

xtest=-10:0.1:10; % define prediction points
opts.maxits = 1000; opts.tol=1e-8;

[meantrain,meantest,logpygx]=GPclass(xtrain,ctrain,xtest,par,'covfnGE',opts);
plot(xtest,meantest,'k.'); hold on
plot(xtrain(ctrain==0),ctrain(ctrain==0),'ro',xtrain(ctrain==1),ctrain(ctrain==1),'g+','linewidth',2,'markersize',8);