function gradloglik=linearCRFgrad(x,y,lambda,A,dimx,dimy)
%LINEARCRFGRAD linear conditional random field gradient
%gradloglik=linearCRFgrad(x,y,lambda,A,dimx,dimy)
%See demoLinearCRF.m
N=length(x);
gradloglik=zeros(length(lambda),1);
for n=1:N
    T=length(y{n}); clear phi
    for t=2:T; phi(t-1)=CRFpotential(x{n},t,lambda,dimx,dimy); end
    % pair marginal probability:
    [marg mess]=sumprodFG(phi,A{n});
    for t=2:T
        indf=CRFfeature(x{n},y{n},t,dimx,dimy);
        if ~isempty(indf)
            gradloglik(indf) = gradloglik(indf) +1;
        end
        [v var2fact fact2var] = VariableConnectingFactor(t-1,A{n});
        potmarg=normpot(multpots([phi(t-1) mess(var2fact)]));% marginal is the potential multiplied by incoming messages
        tab=potmarg.table;
        ytmp=y{n};
        for ytm=1:dimy
            for yt=1:dimy
                ytmp(t-1)= ytm; ytmp(t)= yt;
                indf=CRFfeature(x{n},ytmp,t,dimx,dimy);
                if ~isempty(indf)
                    gradloglik(indf) = gradloglik(indf) - tab(ytm,yt);
                end
            end
        end
    end
end