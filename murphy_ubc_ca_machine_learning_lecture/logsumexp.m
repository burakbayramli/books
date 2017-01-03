function s = logsumexp(b, dim)
% s = logsumexp(b) by Tom Minka
% Returns s(i) = log(sum(exp(b(:,i))))  while avoiding numerical underflow.
% s = logsumexp(b, dim) sums over dimension 'dim' instead of summing over rows

if nargin < 2 % if 2nd argument is missing
  dim = 1;
end

[B, junk] = max(b,[],dim);
dims = ones(1,ndims(b));
dims(dim) = size(b,dim);
b = b - repmat(B, dims);
s = B + log(sum(exp(b),dim));
i = find(~finite(B));
if ~isempty(i)
  s(i) = B(i);
end
