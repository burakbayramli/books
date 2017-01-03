function [ph pvgh loglik phgv]=MIXprodBern(v,H,opts)
%MIXPRODBERN EM training of a Mixture of a product of Bernoulli distributions
%[ph pvgh loglik phgv]=MIXprodBern(v,H,opts)
%
% Inputs:
% v : data matrix : each column is a datapoint
% H : number of mixture components
% opts.maxit
% opts.plotprogress
%
% Outputs:
% ph : p(h)
% pvgh : p(v|h)
% loglik : log likelihood of the set of data
% phgv : p(h|v) posterior assignment of datapoints to mixture components
% See also demoMixBernoulli.m
[D N]=size(v);
ph = condp(rand(H,1)); pvgh=rand(D,H); % random initialisations
if isfield(opts,'meaninit')
    if opts.meaninit
        ph=condp(ones(H,1)); pvgh=repmat(mean(v,2),1,H);
        pvgh=condp(pvgh+0.001*rand(size(pvgh)));
    end
end
for emloop = 1:opts.maxit
    %E-step:
    htot=zeros(H,1);
    vhtot1=zeros(D,H);vhtot0=zeros(D,H);
    loglik=0;
    for n = 1:N
        st0=find(v(:,n)==0);
        st1=find(v(:,n)==1);
        logpold = log(ph)+sum(log(pvgh(st1,:)),1)'+sum(log(1-pvgh(st0,:)),1)';
        poldhgvn=condexp(logpold); phgv(:,n)=poldhgvn;
        % get the stats for the M-step:
        htot=htot+poldhgvn;
        vhtot1(st1,:)=vhtot1(st1,:)+repmat(poldhgvn',length(st1),1);
        vhtot0(st0,:)=vhtot0(st0,:)+repmat(poldhgvn',length(st0),1);
        loglik=loglik+logsumexp(logpold,ones(H,1));
    end
    lik(emloop)=loglik;
    if opts.plotprogress; plot(lik); title('log likelihood'); drawnow; end
    ph=condp(htot); 	pvgh=vhtot1./(vhtot1+vhtot0); % M-step
end