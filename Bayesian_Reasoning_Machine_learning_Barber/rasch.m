function [a d ll]=rasch(X,opts)
%rasch Fit a Rasch model to a binary matrix X
% X contains binary data, coded 0 or 1 (nan if missing)
% opts.eta_a - ability learning rate
% opts.eta_d - difficulty learning rate
% opts.plotprogress
% opts.tol - log likelihood termination tolerance
% see demoRasch.m

% Maximum Likelood training using simple Gradient ascent:
[Q S]=size(X);
a=zeros(1,S); d=zeros(Q,1);

if ~isfield(opts,'eta_a'); opts.eta_a=1; end % alpha learning rate
if ~isfield(opts,'eta_d'); opts.eta_d=1; end % delta learning rate
if ~isfield(opts,'plotprogress') opts.plotprogress=0; end % delta learning rate
if ~isfield(opts,'tol') opts.tol=1e-6; end % termination toleration

ll_old=-1e10;
for loop=1:opts.maxits
    sig = sigma(repmat(a,Q,1) - repmat(d,1,S));
    loglik(loop) = mynansum(mynansum(X.*log(sig)+(1-X).*log(1-sig)));
    
    grada = mynansum(X-sig,1);
    gradd = -mynansum(X-sig,2);
    
    a = a + opts.eta_a*grada/S;
    d = d + opts.eta_d*gradd/Q;
    if abs(loglik(end)-ll_old)<opts.tol; break; end; ll_old=loglik(end);
    
    if opts.plotprogress; plot(loglik); title('log likelihood'); drawnow; end
end