function anew=logsumexp(a,varargin)
%LOGSUMEXP Compute log(sum(exp(a).*b)) valid for large a
% logsumexp(a,<b>)
% if b is missing it is assumed to be 1
% example: logsumexp([-1000 -1001 -998],[1 2 0.5])
if nargin==1
    b=ones(size(a));
else
    b=varargin{1};
    if isscalar(b); b=b*ones(size(a)); end
end
amax=max(a); A =size(a,1);
anew = amax + log(sum(exp(a-repmat(amax,A,1)).*b));