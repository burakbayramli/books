function drawID(prob,util,partialorder,varinf)
%DRAWID plot an Influence Diagram
% drawID(prob,util,partialorder,varinf)
% Chance nodes are ovals. Non-chance (decisions and utilities are boxes)
[probvars decvars]=IDvars(partialorder);
pot=prob;
nvars = length(probvars)+length(decvars);
for i=1:length(decvars)
    tmppot.variables=decvars(i);
    tmppot.table=myones(2*repmat(1,1,length(tmppot.variables)));
    pot(decvars(i))=tmppot;
end
pot=changevar(pot,[probvars decvars],1:nvars);
util=changevar(util,[probvars decvars],1:nvars);
varinf=varinf([probvars decvars]);
pot=pot([probvars decvars]);

probvars=1:length(probvars); decvars=probvars(end)+1:probvars(end)+length(decvars);

count=0;
for i=1:length(util)
    if ~isempty(util(i).table)
        count=count+1;
        tmppot.variables=[nvars+count util(i).variables];
        tmppot.table=myones(2*repmat(1,1,length(tmppot.variables)));
        pot=[pot tmppot];
        if ~isempty(util(i).name)
            varinf(nvars+count).name=['util: ',util(i).name];
        else
            varinf(nvars+count).name=['util: ',num2str(count)];
        end
    end
end

nodetype=ones(1,length(pot));
for i=1:length(varinf)
    label{i} = field2cell(varinf(i),'name');
    if ismember(i,probvars); nodetype(i)=0; end
end

draw_layout(dag(pot),label,nodetype);