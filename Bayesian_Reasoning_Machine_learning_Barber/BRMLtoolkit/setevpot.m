function newpot = setevpot(pot,evvariables,evidstates)
%SETEVPOT Sets variables in a potential into evidential states
% newpot = setevpot(pot,evvariables,evidstates)
% returns a potential on the same set of variables as pot with any
% evidential variables set to their evidence states.
% The potential is then multiplied by the uniform potential on the evidential variables.
% This routine is useful for example when calling a Junction Tree with different sets
% of evidence. In this way the structure of the Junction Tree remains the same.
% The BRML software does not make use of global variable information and in this way
% information about the number of states of the evidential variables is
% retained. This contrasts with setpot.m in which evidential variables are removed.
% see also setpot.m demoJTree.m
[newpot]= setpot(pot,evvariables,evidstates);
[variables nstates]=potvariables(pot);
for p=1:length(pot)
    [dum toelim]=ismember(evvariables,pot(p).variables);
    if sum(toelim>0)
        elimvar=evvariables(find(toelim));
        [a b]=ismember(elimvar,variables);
        ns=nstates(b);
        epot.variables=elimvar;
        epot.table=myones(ns)./prod(ns);
        newpot(p)=multpots([newpot(p) epot]);
    end
end