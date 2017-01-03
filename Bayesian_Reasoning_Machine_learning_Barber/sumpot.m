function newpot = sumpot(pot,varargin)
%SUMPOT Sum potential pot over variables
% newpot = sumpot(pot,<variables>,<sumover>)
%
% if called with the additional argument: sumpot(pot,variables,sumover)
% if sumover=1 then potential is summed over variables, otherwise the
% potential is summed over everything except variables
% if called as sumpot(pot), it is assumed that all variables of pot are to be summed over
sumover=1; % default setting
if nargin==1; invariables=[]; sumover=0;
else
    invariables=varargin{1}; 
end
if length(varargin)==2
    sumover=varargin{2};
end
for p=1:length(pot)
    if sumover
        variables=invariables;  % variables that will be summed over
        newpot(p).variables = setminus(pot(p).variables,variables); % new potential variables
    else
        variables=setdiff(pot(p).variables,invariables); % variables that will be summed over
        newpot(p).variables = setminus(pot(p).variables,variables); % new potential variables
    end
    table_variables=find(ismember(pot(p).variables,variables));   % indices of the table that will be summed over
    if potistyped(pot(p))
        newpot.table=feval(['sumpot' pot.table.type],pot,table_variables); % execute specific sum function if required
        % note that the specific sumpotX functions take a table (not a
        % potential) as input and the specific indices of the table that
        % are required to be summed over. See for example sumpotGaussianMoment.m
        % This mechanism allows one to extend the toolbox by constructing
        % specific summation/marginalisation operators for any potential type.
    else
        [dum nstates]=potvariables(pot(p));
        s = size(pot(p).table);
        if length(nstates)==1 && s(1)==1 % if there is only one variable it might be stored as a row vector
            t=pot(p).table'; % flip to make row vector
        else
            t=pot(p).table; % get the table array
        end
        for v=table_variables
            t=sum(t,v); % resursively sum over the variables
        end
        newpot(p).table=squeeze(t); % remove any redunant vacuous remaining indices
        s = size(newpot(p).table);
        if s(1)==1; newpot(p).table=newpot(p).table'; end % return column vector if possible
    end
end