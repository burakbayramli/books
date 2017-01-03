function [F,diagPsi,m,loglik]=FA(X,H,varargin)
%FA Factor Analysis
% [F,diagPsi,m,loglik]=FA(X,H,<opts>)
%
% No post rotation of the factors F is applied
%
% Inputs:
% X is the D x N data matrix with each column being a D-dimensional datapoint
% H is the number of factors
% opts.init.diagPsi: initial D dimensional vector describing the diagonal covariance matrix
% opts.init.F : initial Factor loading matrix (ignored if opts.init.diagPsi exists)
% opts.maxit: maximum number of iterations (default is 100)
% opts.tol : change in log likelihood termination criterion (default is 10e-5)
% opts.plotprogress: set to 1 to display log likelihood (default is 0)
%
% Outputs:
% F : factor loading matrix
% diagPsi : diagonal entries of the covariance
% m : mean of the data
% loglik: log likelihood of the data
opts=[];if nargin==3; opts=varargin{1}; end
opts=setfields(opts,'tol',1e-5,'maxit',100,'plotprogress',1);% default options
[V N] = size(X);
m = mean(X,2);
X = X - repmat(m,1,N); diagS = var(X,1,2);
if isfield(opts,'init');
    if isfield(opts.init,'diagPsi')
        diagPsi=opts.init.diagPsi;
    elseif isfield(opts.init,'F')
        diagPsi=diagS-sum(opts.init.F.^2,2);
    end
else
    diagPsi=ones(V,1);
end
for loop=1:opts.maxit
    diagSqrtPsi = sqrt(diagPsi)+10e-10;
    Xtilde = diag(1./diagSqrtPsi)*X/sqrt(N);
    [U,LambdaTilde] = svd(Xtilde,0);
    Uh = U(:,1:H); diagLambda=diag(LambdaTilde).^2;
    F = diag(diagSqrtPsi)*Uh*diag(sqrt(max(diagLambda(1:H)-1,0)));
    loglik=-0.5*N*(sum(log(diagLambda(1:H)))+H+sum(diagLambda(H+1:end))+V*log(2*pi)+sum(log(diagPsi)));
    logpV(loop) = loglik;
    if loop>1
        if  logpV(loop) - logpV(loop-1) < opts.tol; break; end
    end
    if opts.plotprogress;    plot(logpV,'-o'); title('Log likelihood'); drawnow; end
    diagPsi=diagS-sum(F.^2,2);
end