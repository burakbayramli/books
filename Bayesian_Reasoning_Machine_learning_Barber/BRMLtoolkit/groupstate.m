function [gpstate gpvariables]=groupstate(group,vars,state)
%GROUPSTATE find the state of the group variables corresponding to a given ungrouped state.
%
% [gpstate gpvariables]=groupstate(group,vars,state)
% group(i).variables contains the variables in group i
% vars is a vector of ungrouped variables, and state a vector of their corresponding states
% See also ungroupstate.m, ungrouppot.m, grouppot.m, demogrouppot.m
for i=1:length(vars)
    thisvar=vars(i);
    for g=1:length(group)
        gv=group(g).variables;
        if ismember(thisvar,gv)
            gvars(g)=1;
            gg(find(gv==thisvar),g)=state(i);
        end
    end
end
gpvariables=find(gvars);
nstates=[]; vstate=[]; gnstates=[];
for g=gpvariables
    if prod(gg(:,g))==0; error(['insufficient state specification in group variable' num2str(g)]);
    else
        nstates=[nstates (group(g).nstates(:))'];
        vstate=[vstate gg(:,g)'];
        gnstates=[gnstates prod(group(g).nstates)];
    end   
end
ind=subv2ind(nstates,vstate);
gpstate=ind2subv(gnstates,ind);