function newpot = deltapot(evvariables,evidstates,nstates_evvariables)
%DELTAPOT A delta function potential 
% newpot = deltapot(evvariables,evidstates,nstates_evvariables)
% Inputs: 
% evvariables : variables that are evidential
% evidstates  : the corresponding states
% nstates_evvariables): number of states of the evidential variables
% Output :
% newpot : a delta function potential over evvariables, with mass only in the evidential states
tmp=zeros(prod(nstates_evvariables),1);
tmp(subv2ind(nstates_evvariables,evidstates(:)'))=1;
if isempty(evvariables)
    newpot.table=1;
else
    newpot.table=tmp();
end
newpot.variables=evvariables;
if length(evvariables)>1; newpot.table=reshape(newpot.table,nstates_evvariables); end