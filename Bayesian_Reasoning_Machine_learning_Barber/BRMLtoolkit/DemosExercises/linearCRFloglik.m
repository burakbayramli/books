function loglik=linearCRFloglik(x,y,lambda,A,dimx,dimy)
%LINEARCRFLOGLIK Linear conditional random field log likelihood
%loglik=linearCRFloglik(x,y,lambda,A,dimx,dimy)
% See demoLinearCRF.m
N=length(x);loglik=0;
for n=1:N
    T=length(y{n}); clear phi
    for t=2:T
        phi(t-1)=CRFpotential(x{n},t,lambda,dimx,dimy);
    end
    % partition function:
    [marg mess]=sumprodFG(phi,A{n},[]);
    [f fact2var var2fact] = FactorConnectingVariable(1,A{n});
    logZ = log(table(sumpot(multpots(mess(fact2var)),[],0)));
    for t=2:T
        loglik = loglik + sum(lambda(CRFfeature(x{n},y{n},t,dimx,dimy)));
    end
    loglik=loglik-logZ;
end