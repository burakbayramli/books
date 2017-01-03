function [probvars decvars]=IDvars(partialorder)
%IDVARS probability and decision variables from a partial order
% [probvars decvars]=IDvars(partialorder)
probvars=[]; decvars=[];
for i=1:length(partialorder)
    s = partialorder{i};
    if isfield(s,'sum')
        probvars=[probvars s.sum];
    else
        decvars=[decvars s.max];
    end
end