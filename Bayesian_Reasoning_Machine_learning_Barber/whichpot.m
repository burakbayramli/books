function potnum = whichpot(pot,variables,varargin)
%WHICHPOT Returns potentials that contain a set of variables
% potnum = whichpot(pot,variables,<n>)
%
% Return potential numbers that contain all the specified variables
% The cliques are returned with those containing the smallest number of
% variables first.
% If optional n is used, returns at most n potential numbers 
potnum=[]; nvars=[];
for p=1:length(pot)
    % find the potential that contains variables
    if prod(real(ismember(variables,pot(p).variables)))
        nvars=[nvars length(pot(p).variables)];
        potnum=[potnum p];
    end
end
[val ind]=sort(nvars);
if isempty(varargin); n=length(potnum);
else n=min([length(potnum) varargin{1}]);
end
potnum=potnum(ind(1:n));