function [bl, bu] = sensitivityAnalysisb(A, b, BasicList)
% Filename: sensitivityAnalysisb.m
% Description: the function is an implementation of the 
% sensitivity analysis for the right-hand side 
% coefficients
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [bl, bu] = sensitivityAnalysisb(A, b, BasicList)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
% -- BasicList: vector of the indices of the basic 
%    variables (size 1 x m)
%
% Output:
% -- bl: vector of the lower values of the range of the 
%    right-hand side coefficients (size 1 x m)
% -- bu: vector of the upper values of the range of the 
%    right-hand side coefficients (size 1 x m)

m = length(BasicList); % find the number of constraints
% initialize output
bl = zeros(1, m);
bu = zeros(1, m);
BasisInv = inv(A(:, BasicList)); % invert the basis
Xb = BasisInv * b; % compute vector Xb
% calculate the lower and upper values of the range 
% of the right-hand side coefficients
for i = 1:m
	% find the positive values in the basis inverse
	% of the specific constraint
	t = find(BasisInv(:, i) > 0);
	if isempty(t) % if no positive value exists
        bl(i) = -Inf;
	else % calculate the lower value
        bl(i) = b(i) + max(-Xb(t) ./ BasisInv(t, i));
	end
	% find the negative values in the basis inverse
	% of the specific constraint
	t = find(BasisInv(:, i) < 0);
	if isempty(t) % if no negative value exists
        bu(i) = Inf;
	else % calculate the upper value
        bu(i) = b(i) + min(-Xb(t) ./ BasisInv(t, i));
	end
end
end