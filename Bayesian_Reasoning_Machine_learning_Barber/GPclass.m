function [meantrain,meantest,logpygx]=GPclass(xtrain,ctrain,xtest,par,covfn,opts)
%GPCLASS Gaussian Process Binary Classification
% [meantrain,meantest,logpygx]=GPclass(xtrain,ctrain,xtest,par,covfn,opts)
% see also demoGPclass
N=size(xtrain,2);
train=1:N; test=(1:size(xtest,2))+N;
K = feval(covfn,[xtrain xtest],[xtrain xtest],par); % Covariance function for noisy outputs

ctrain=ctrain(:); % make a column vector
y=zeros(N,1);
for wloop=1:opts.maxits % Newton update for Laplace approximation
    sigvec=sigma(y);
    D = diag(sigvec.*(1-sigvec));
    tmp = D*y+ ctrain-sigvec;
    Mat=eye(N)+K(train,train)*D;
    yold = y;
    y=K(train,train)*(Mat\tmp);
    if mean(abs(y-yold))<opts.tol; break; end
end
meantrain=sigvec;

% compute predictions:
cms = ctrain-sigvec; invD = diag(1./diag(D));
for n=1:size(xtest,2)
    k = K(train,test(n));
    kss = K(test(n),test(n));
    mn = k'*cms;  % mean of the field projection
    vr = kss - k'*((K(train,train)+invD)\k); % variance
    meantest(1,n)=avsigmaGauss(mn,vr);
end
logpygx = ctrain'*y - sum(log(1+exp(y))) - 0.5*y'*cms-0.5*logdet(Mat); % training data log likelihood