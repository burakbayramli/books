function [index] = lrcm(Sn, NonBasicList, lrcmLast, ...
    iteration)
% Filename: lrcm.m
% Description: the function is an implementation of the 
% Least Recently Considered Method pivoting rule
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [index] = lrcm(Sn, NonBasicList, lrcmLast, ...
%   iteration)
% 
% Input:
% -- Sn: vector of reduced costs (size 1 x (n - m))
% -- NonBasicList: vector of indices of the nonbasic 
%    variables (size 1 x (n - m))
% -- lrcmLast: the index of the last selected entering 
%    variable
% -- iteration: the current iteration of the simplex 
%    algorithm
%
% Output:
% -- index: the index of the entering variable

% use Dantzig's rule in the first iteration
if iteration == 1
	[~, index] = min(Sn);
else
	% searching for the first eligible variable with index 
	% greater than lrcmLast
	temp1 = find(Sn < 0);
	temp2 = NonBasicList(temp1);
	temp3 = find(temp2 > lrcmLast);
	% if not any eligible variable with index greater than 
	% lrcmLast, start searching from the first column again
	if isempty(temp3)
        [~, b] = min(temp2);
        index = temp1(b);
    else
        index = temp1(min(temp3));
	end
end
end