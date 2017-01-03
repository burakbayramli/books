function varnames=blanknames(varargin)
%BLANKNAMES Create empty variables names
% create empty variable names eg varnames([1  3])=blanknames or
% varnames=blanknames(1:3)
if isempty(varargin)
varnames{1}={''};
else
    vars=varargin{1};
    for i=vars
        varnames{i}={''};
    end
end