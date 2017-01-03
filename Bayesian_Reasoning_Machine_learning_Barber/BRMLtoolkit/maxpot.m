function [newpot maxstate] = maxpot(pot,invariables,varargin)
%MAXPOT Maximise a potential over variables
% [newpot maxstate] = maxpot(pot,invariables,<maxover>)
% 
% Inputs:
% pot : a potential
% invariables : the variables to optimise over
% By default we maximise over invariables.
% if <maxover>=1 then potential is maxed over variables, otherwise the
% potential is maxed over everything except variables. For example, to find
% the maximum value and state over all variables, use [newpot maxstate] = maxpot(pot,[],0) 
% 
% Outputs:
% newpot : remaining potential after performing the maximisation
% maxstate : returns the states (of the variables maxed over)
if length(pot)>1; error('maxpot can only handle a single potential','brml'); end
maxover=1;
if length(varargin)==1
	maxover=varargin{1};
end
if maxover
	variables=invariables;
	newpot.variables = setdiff(pot.variables,variables); % new potential variables
else
	variables=setdiff(pot.variables,invariables);
	newpot.variables = setdiff(pot.variables,variables); % new potential variables
end
st_variables =find(ismember(pot.variables,variables));
[dum nstates]=potvariables(pot);
[newpot.table maxstate_all]=maxarray(pot.table,st_variables);
maxstate=maxstate_all(:,st_variables); % just return the states of variables maxed over