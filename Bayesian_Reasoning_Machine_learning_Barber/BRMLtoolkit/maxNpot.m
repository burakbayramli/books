function [newpot maxstate] = maxNpot(pot,invariables,N,varargin)
%MAXNPOT Find the N most probable values and states in a potential
% [newpot maxstate] = maxNpot(pot,variables,N,<maxover>)
%
% max potential pot over variables
% returns the N most probable values and states (of the variables maxed over)
% if called with the additional argument: maxpot(pot,variables,N,maxover)
% if maxover=1 then potential is maxed over variables, otherwise the
% potential is maxed over everything except variables. For example, to find
% the maximum value and state over all variables, use
% [newpot maxstate] = maxpot(pot,[],N,0)
maxover=1;
if length(varargin)==1
	maxover=varargin{1};
end
if maxover
	variables=invariables;
	[newpot.variables newpot_variables]= setdiff(pot.variables,variables); % new potential variables
else
	variables=setdiff(pot.variables,invariables);
	[newpot.variables newpot_variables]= setdiff(pot.variables,variables); % new potential variables
end
st_variables =find(ismember(pot.variables,variables));
[dum nstates]=potvariables(pot);
[newpot.table maxstate_all]=maxNarray(pot.table,st_variables,N);
for i=1:N
	maxstate{i}=maxstate_all{i}(:,st_variables); % just return the states of variables maxed over
end