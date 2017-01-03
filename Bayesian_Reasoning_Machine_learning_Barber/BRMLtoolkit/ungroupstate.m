function [variable state]=ungroupstate(group,gpvar,gpstate)
%UNGROUPSTATE find the state of the variables corresponding to a given grouped state.
%
% [variable state]=ungroupstate(group,gpvar,gpstate)
% group(i).variables contains the variables in group i
% See also ungrouppot.m, grouppot.m, demogrouppot.m 
state=ind2subv(group(gpvar).nstates,gpstate);
variable=group(gpvar).variables;