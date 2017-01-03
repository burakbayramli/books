function newpot = multpots(pots)
%MULTPOTS Multiply potentials into a single large potential
% newpot = multpots(pots)
%
% multiply potentials : pots is a structure array of potentials
pots=orderpotfields(pots); numpots = length(pots);
newpot=struct('variables',[],'table',[]);
[newpot.variables nstates con convec]=potvariables(pots);
if con==0
    warning(['There is a dimension conflict for variable(s) ',num2str(newpot.variables(~convec))]);
end
[typed types]=potistyped(pots);
if typed
    newpot=pots(1);
    for j=2:numpots % loop over all the potentials
        type1=newpot.table.type; % type of the existing potential
        type2=types(j); % type of the potential to include in the product
        if strcmp(type1,type2)
            newpot=feval(['multpots' type1],newpot,pots(j)); 
        elseif exist(gatherstrings(['multpots' type1 'x' type2]))
            newpot=feval(gatherstrings(['multpots' type1 'x' type2]),newpot,pots(j));
        elseif exist(gatherstrings(['multpots' type2 'x' type1]))
            newpot=feval(gatherstrings(['multpots' type2 'x' type1]),newpot,pots(j));
        else
            warning(gatherstrings(['The function multpots' type1 'x' type2 '.m is not defined']))
        end
    end
else
    p=myones(nstates);
    for j=1:numpots % loop over all the potentials
        vars=pots(j).variables;
        %        if ~isempty(vars) & ~isempty(pots(j).table)
        if ~isempty(pots(j).table)
            pots(j)=orderpot(pots(j),sort(vars));
            [dummy thispotvarind]=ismember(pots(j).variables,newpot.variables);
            s = size(pots(j).table);
            if length(s)==2 && s(1)==1
                pots(j).table=pots(j).table'; % for a 1 var potential, might be stored as row vector
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
            p=p.*t;
        end
    end
    newpot.table=p;
end