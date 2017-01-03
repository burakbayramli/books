function [maxval maxstate]=maxarray(x,maxover,varargin)
%MAXARRAY Maximise a multi-dimensional array over a set of dimenions
% [maxval maxstate]=maxarray(x,maxover)
% find the values and states that maximize the multidimensional array x
% over the dimensions in maxover
%
% If called as maxarray(x,maxover,nstates), x can be in linear index form,
% and nstates decribes the dimensions of the variables in the linear index x
%
% One can view this function as returning, for non-optimised joint index,
% the optimal value of the array, maximised over the maxover indices.
%
% Example:[maxval maxstate]= maxarray(randn(3,4,2),[3 1])
% In this case we maximise over the 3rd and 1st dimensions, of the 3x4x2
% table. This means that after maximising, the remaining array is a 4x1 array.
% maxval(i) contains the maximum value of the array when the second dimension
% is in state i.
% See also maxNarray.m
if isempty(varargin)
    dimvars=size(x);
else
    dimvars=varargin{1};
    if length(dimvars)>1; x=reshape(x,dimvars); end
end
if nargout<=1
    for i=1:length(maxover); x=max(x,[],maxover(i)); end % works faster if just need max value
    maxval=squeeze(x);
else
    nvars=length(dimvars); left=setdiff(1:nvars,maxover);
    dimleft = prod(dimvars(left)); dimover =prod(dimvars(maxover));
    if length(dimvars)>1; xx=permute(x,[left maxover]); else xx=x; end
    s = reshape(xx,dimleft,dimover);
    [maxval b]=max(s,[],2);  c = [ (1:dimleft)' b(:)];
    maxstate = ind2subv(dimvars([left maxover]),subv2ind([dimleft,dimover],c));
    [d p]=sort([left maxover]); maxstate=maxstate(:,p);
    if isempty(varargin) && length(left)>1
        maxval=reshape(maxval,dimvars(left)); % return in array, otherwise leave as linear index
    end
end