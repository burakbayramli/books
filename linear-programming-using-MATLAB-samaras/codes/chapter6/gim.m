function [index] = gim(Sn, NonBasicList, A, ...
    BasisInv, xb, tole)
% Filename: gim.m
% Description: the function is an implementation of the 
% Greatest Increment Method pivoting rule
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [index] = gim(Sn, NonBasicList, A, ...
%   BasisInv, xb, tole)
% 
% Input:
% -- Sn: vector of reduced costs (size 1 x (n - m))
% -- NonBasicList: vector of indices of the nonbasic 
%    variables (size 1 x (n - m))
% -- A: matrix of coefficients of the constraints 
%    (size m x n) 
% -- BasisInv: matrix with the basis inverse (size m x m)
% -- xb: vector with the values of the basic variables
%    (size m x 1)
% -- tole: the value of the tolerance for the pivoting step
% 
% Output:
% -- index: the index of the entering variable

index = -1;
maxDecrease = 1;
% calculate the improvement of the objective value for each
% nonbasic variable
for i = 1:length(NonBasicList)
	if Sn(i) >= 0 % only consider the eligible variables
        continue;
	end
	% store index of the candidate variable
	l = NonBasicList(i);
	hl = BasisInv * A(:, l); % compute the pivot column
	% set equal to zero the values of the pivot column that 
	% are less than or equal to the given tolerance
	toler = abs(hl) <= tole;
	hl(toler == 1) = 0;
	mrt = find(hl > 0);
	% if there is not any variable in the pivot column 
	% greater than zero, then the problem is unbounded
	if numel(mrt) < 1
        index = -1;
        return;
	end
	% calculate the total objective value improvement
	xbDIVhl = xb(mrt) ./ hl(mrt);
	theta0 = min(xbDIVhl(xbDIVhl > 0));
	currDecrease = theta0 * Sn(i);
	% if this improvement is greater than the current 
	% maximum improvement, store the value and the index
	if currDecrease < maxDecrease
        maxDecrease = currDecrease;
        index = i;
	end
end
% if the entering variable is empty, then find the index 
% of the variable with the most negative Sn
if index < 0
	[~, index] = min(Sn);
end
end