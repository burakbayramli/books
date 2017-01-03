function [variables nstates con convec varinf]=potvariables(pot)
%POTVARIABLES Returns information about all variables in a set of potentials
% [variables nstates con convec]=potvariables(pot)
%
% return the variables and their number of states
% If there is a dimension mismatch in the tables then return con=0
% convec(i)=0 reports that variable i has conflicting dimension
variables=[];nstates=[];con=1;convec=[];varinf=[];
if length(pot)==0;return;end
v=cat(2,pot.variables);
if ~isempty(v)
    [a b]=sort(v); i=[b(diff(a)>0) b(end)];variables=v(i);
    N=max(variables);
    convec=ones(1,N);nstates=-ones(1,N);
    for p=1:length(pot)
        if ~isfield(pot(p).table,'type')
            if ~isempty(pot(p).variables) && length(pot(p).variables)~=length(numstates(pot(p)))
                warning(['potential ',num2str(p),' has conflicting number of variables and table dimension']);
            end
        end
        nstates(1,pot(p).variables) = numstates(pot(p));
        if p>1
            convec(nstates(oldnstates>-1)~=oldnstates(oldnstates>-1))=0;
        end
        oldnstates=nstates;
    end
    con = all(convec); if con==0; warning('variable dimensions inconsistent'); end
else
    variables=[]; nstates=[];
end
nstates=nstates(nstates>0);
if potistyped(pot(1))
    varinf.pottype=pot(1).table.type;
else
    varinf.pottype=[];
end