function x = sample(p, n)
%SAMPLE    Sample from categorical distribution.
% SAMPLE(P,N) returns a row vector of N integers, sampled according to the 
% probability distribution P (an array of numbers >= 0, whose sum is > 0).
% sum(P) does not have to be 1, but it must be > 0.

% Written by Tom Minka
% (c) Microsoft Corporation. All rights reserved.

if nargin < 2
  n = 1;
end

cdf = cumsum(p(:));
if cdf(end) <= 0
  error('distribution is all zeros');
 end
for i = 1:n
  x(i) = sum(cdf < rand*cdf(end)) + 1;
end
