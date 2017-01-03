function demoGPclass
%DEMOGPCLASS:
% demo of Gaussian Process Binary Classification using Laplace with fixed
% hyperparameters. Note that this is not optimally encoded for speed nor accuracy

% make some training data
M=20;
xtrain=randn(2,M); % class 1
xtrain=[xtrain (randn(2,M)+2.5*ones(2,M))]; % class 0
xtrain=[xtrain (randn(2,M)-2.5*ones(2,M))]; % class 0
ctrain(M+1:3*M)=1; ctrain(1:M)=0; % training class labels
N=3*M; % total number of training points

% define the Kernel matrix:
par(1).value=1; % prefactor
par(2).value=2; % inverse root lengthscale
par(3).value=2; % gamma exponential
par(4).value=0.001; % jitter
% define predictions for on a very coarse grid  (note that matlab doesn't interpolate intelligently)
xx=-5:0.25:5; yy=-5:0.25:5;
xtest=[];
for i=1:length(xx)
    for j=1:length(yy)
        xtest=[xtest [xx(i) yy(j)]'];
    end
end

opts.maxits = 100; opts.tol = 1e-8;
[meantrain,meantest,logpygx]=GPclass(xtrain,ctrain,xtest,par,'covfnGE',opts);
plot(meantrain,'o'); hold on; 	plot(ctrain,'r+'); title('training class labels and fit'); drawnow; hold off;

figure; hold on
for n=1:N
    if ctrain(n)==0;
        plot(xtrain(1,n),xtrain(2,n),'ro','markersize',5);
    else
        plot(xtrain(1,n),xtrain(2,n),'gd','markersize',5);
    end
    if meantrain(n)<0.5;
        plot(xtrain(1,n),xtrain(2,n),'ro','markersize',8);
    else
        plot(xtrain(1,n),xtrain(2,n),'gd','markersize',8);
    end
end
[cs,hnd]=contour(xx,yy,reshape(meantest,length(xx),length(yy))); clabel(cs,hnd,'fontsize',10); axis([-5 5 -5 5]);