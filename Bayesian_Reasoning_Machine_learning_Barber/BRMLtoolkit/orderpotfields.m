function outpot=orderpotfields(pot)
%ORDERPOTFIELDS Order the fields of the potential, to place `variables' and
%`table' first and second and remove any other fields
% pot=orderpotfields(pot)
for p=1:length(pot);
    if ~isfield(pot(p),'variables')
       outpot(p).variables=[];
    end
    if ~isfield(pot(p),'table')
        outpot(p).table=[];
    end
    outpot(p).variables=pot(p).variables;
    outpot(p).table=pot(p).table;
end