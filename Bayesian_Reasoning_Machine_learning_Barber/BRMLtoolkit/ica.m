function [A B X]=ica(Y,opts)
% ICA Independent Components Analysis using maximum likelihood, prior=1/cosh
% [A B X]=ica(Y,opts)
% Inputs:
% Y is the data
% opts.maxits : maximum iterations
% opts.tol : termination criterion
% opts.plotprogress=1 : plot the progress
% opts.eta : learning rate : default =0.5
% opts.whiten = 1 : whiten the data
% Outputs :
% A : square mixing matrix (B is the inverse of A)
% X : latent values
[D N]=size(Y);
if ~isfield(opts,'eta'); opts.eta=0.5; end
if ~isfield(opts,'plotprogress'); opts.plotprogress=0; end
if opts.whiten
    Y=Y-repmat(mean(Y,2),1,N);
    [U S V]=svd(Y);
    Q = sqrt(N)*diag(1./(diag(S(1:D,1:D))))*U';
    Z=Q*Y; % initialise to have unit cross moment
else
    Z=Y; Q=eye(D);
end
B=eye(D);
for it=1:opts.maxits
    X=B*Z;
    Bnew = B + opts.eta*(B-tanh(X)*(X'*B)/N);
    if mean(abs(Bnew(:)-B(:)))<opts.tol
        break
    end
    B=Bnew;
    logl(it) = N*logdet(B) - sum(sum(log(cosh(B*Z))));
    if opts.plotprogress; plot(logl); title('log likelihood'); drawnow; end
end
B = B*Q; A = inv(B);