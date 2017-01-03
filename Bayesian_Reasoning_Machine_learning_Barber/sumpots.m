function newpot = sumpots(pots)
%SUMPOTS Sum a set of potentials
% newpot = sumpots(pots)
% The returned potential contains the summation of all potentials pots
pots=orderpotfields(pots); numpots = length(pots);
[newpot.variables nstates]=potvariables(pots);
p = myzeros(nstates);
for j=1:numpots % loop over all the potentials
    vars=potvariables(pots(j));
    if ~isempty(vars)
        pots(j)=orderpot(pots(j),sort(vars));
        [dummy thispotvarind]=ismember(pots(j).variables,newpot.variables);
        s = size(pots(j).table);
        if length(s)==2 && s(1)==1
            pots(j).table=pots(j).table';
        end
        r = nstates; r(thispotvarind)=1;
        q = ones(1,length(nstates));
        q(thispotvarind)=numstates(pots(j));
        if length(q)>1
            t = reshape(pots(j).table,q);
            t = reshape(repmat(t,r),nstates);
        else
            t=pots(j).table(:);
        end
        p=p+t;
    end
end
newpot.table=p;