function x=ancestralsample(pot,nsamples)
%ANCESTRALSAMPLE Ancestral sampling from a Belief Network
% x=ancestralsample(pot,nsamples)
%
% returns samples from a Belief Network contained in pot using ancestral
% sampling. Each sample is in order (x_1,x_2,...x_V), where V is the number
% of variables in the Belief Network.  pot(i) must contain p(i|pa(i))
[ord noparents]=ancestralorder(dag(pot));
[variables nstates]=potvariables(pot);
for samp=1:nsamples
    % first sample from nodes with no parents
    prevvars=[]; prevstates=[];
    for v=noparents
        randindex = randgen(pot(v).table);
        rs = ind2subv(nstates(pot(v).variables),randindex);
        prevvars=[prevvars v]; prevstates=[prevstates rs]; % add the variables and states to the set of sample
    end
    for v=ord(length(noparents)+1:end)
        tmp=setpot(pot(v),prevvars,prevstates);
        randindex = randgen(tmp.table);
        rs = ind2subv(nstates(tmp.variables),randindex);
        prevvars=[prevvars v]; prevstates=[prevstates rs]; % add the variables and states to the set of sample
    end
    x(prevvars,samp)=prevstates';
end