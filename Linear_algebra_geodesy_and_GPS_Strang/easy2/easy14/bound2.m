function [index] = bound2(percent, data, total)
%BOUND2	Percentile bound on a histogram
%	[INDEX] = bound2(Percentile, X, total) finds the index of the 
%   one-sided vextor histogram, X, which bounds Percentile 
%   events. For matrix X BOUND finds said index of each column 
%   and returns a vector of indices. Optional total argument can be used
%   instead of column total

%	See also: TMSSTAT, SIGSTAT, BOUND

%	AJHansen 5 November 1996
%   modified by Todd Walter 11 June 1997

if nargin < 2
  error('Must input a percent and the data')
end
if nargin < 3
  total = sum(data);
end
[m n] = size(data);
index = zeros(1,n);
bnd_cnt = ceil(percent*total);

for idx = 1:n
    include = 0;
    start = 1;
    curr = data(start,idx);
    while (curr < bnd_cnt(idx)) & (include < m-start)
        include = include + 1;
        curr = curr + data(start+include,idx);
    end
    index(idx) = start + include;
end
