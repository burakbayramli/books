function out = evalpot(pot,varargin)
%EVALPOT Evaluate the table of a potential when variables are set
% out = evalpot(pot,<evvariables>,<evidence>)
% Evaluate the table of potential pot when evvariables are set to evidence
% Without the optional inputs, simply return the table of pot
if length(varargin)==2
	evvariables=varargin{1};
	evidence=varargin{2};
	% set variables in potential to evidential states in evidstates
	nstates=numstates(pot);
	[dum ind]=ismember(pot.variables,evvariables);
	ev=evidence(ind);
	tableind = subv2ind(nstates,ev(:)'); % index of evidence
	out=pot.table(tableind);
else
	out=pot.table;
end