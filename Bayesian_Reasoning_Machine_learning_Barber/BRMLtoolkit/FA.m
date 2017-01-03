function [F,diagPsi,m,loglik]=FA(X,H,varargin)
%FA Factor Analysis
% [F,diagPsi,m,loglik]=FA(X,H,<opts>)
%
% No post rotation of the factors F is applied
%
% Inputs:
% X is the D x N data matrix with each column being a D-dimensional datapoint
% H is the number of factors
% opts.init.F : initial Factor D x H loading matrix
% opts.init.diagPsi: initial D dimensional vector describing the diagonal covariance matrix
% opts.maxit: maximum number of iterations
% opts.tol : change in log likelihood termination criterion
% opts.plotprogress: set to 1 to display log likelihood
%
% Outputs:
% F : factor loading matrix
% diagPsi : diagonal entries of the covariance
% m : mean of the data
% loglik: log likelihood of the data
opts=[];if nargin==3; opts=varargin{1}; end
opts=setfields(opts,'tol',1e-5,'maxit',20,'plotprogress',0);% default options
[V N] = size(X);
m = mean(X,2);
X = X - repmat(m,1,N); diagS = var(X,1,2);
if isfield(opts,'init');
    F=opts.init.F;    diagPsi=opts.init.diagPsi;
else
    F = randn(V,H); diagPsi=ones(V,1);
end
tmpold=-realmax;
for loop=1:opts.maxit
    diagSqrtPsi = sqrt(diagPsi)+10e-10;
    Xtilde = diag(1./diagSqrtPsi)*X/sqrt(N);
    [U,LambdaTilde] = svd(Xtilde,0);
    Uh = U(:,1:H); diagLambda=diag(LambdaTilde).^2;
    F = diag(diagSqrtPsi)*Uh*diag(sqrt(max(diagLambda(1:H)-1,0)));
    loglik=-0.5*N*(sum(log(diagLambda(1:H)))+H+sum(diagLambda(H+1:end))+V*log(2*pi)+sum(log(diagPsi+eps)));
    logpV(loop) = loglik;
    if loop>1
        if  logpV(loop) - logpV(loop-1) < opts.tol; break; end
    end
    if opts.plotprogress;    plot(logpV,'-o'); title('Log likelihood'); drawnow; end
    diagPsi=diagS-sum(F.^2,2);
end