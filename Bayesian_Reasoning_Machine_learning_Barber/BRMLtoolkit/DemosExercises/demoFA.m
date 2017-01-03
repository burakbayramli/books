function demoFA
%DEMOFA demo of Factor Analysis
figure;
% generate some training data (from a FA model):
F0=randn(5,3); h=randn(3,20); X=F0*h+0.01*randn(5,20);
opts.maxit = 50; opts.plotprogress=1; opts.tol=0.1; H=3;
[F,diagPsi,m,loglik]=FA(X,H,opts);
subplot(1,2,1); imagesc(cov(X',1)); title('Sample Covariance')
subplot(1,2,2); imagesc(F*F'+diag(diagPsi)); title('FA Covariance')