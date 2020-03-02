function [Sn] = partialPricingUpdate(A, c, BasicList, ...
	NonBasicList, BasisInv, tole, segmentSize)
% Filename: partialPricingUpdate.m
% Description: the function is an implementation of the 
% update routine for the static partial pricing method 
% using Dantzig's pivoting rule
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [index] = partialpricingupdate(Sn, A, BasicList, ...
%	NonBasicList, BasisInv, tole, segmentSize)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n) 
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
% -- BasicList: vector of indices of the basic variables
%   (size 1 x m)
% -- NonBasicList: vector of indices of the nonbasic 
%    variables (size 1 x (n - m))
% -- BasisInv: matrix with the basis inverse (size m x m)
% -- tole: the value of the tolerance for the pivoting step
% -- segmentSize: the size of the segment

% Output:
% -- Sn: vector of reduced costs (size 1 x (n - m))
%
% initialize vectors cb, cn, w and Sn
cb = c(BasicList);
cn = c(NonBasicList);
w = cb * BasisInv;
Sn = zeros(size(cn), 1);
currentSegment = 1; % initialize current segment
negativeFound = false;
Nend = 0;
% stop when a negative value is found in Sn or when we 
% finished searching all segments
while negativeFound == false && Nend < length(cn)
	% the start of the segment
	Nstart = (currentSegment - 1) * segmentSize + 1;
	% the end of the segment
	Nend = Nstart + segmentSize - 1;
	% if the end is greater than the length of Sn, set 
	% Nend = length(Sn)
	if Nend > length(cn)
		Nend = length(cn);
	end
	% calculate Sn
	N = A(:, NonBasicList);
    HRN = w * N(:, Nstart:Nend);
	Sn(Nstart:Nend) = cn(Nstart:Nend) - HRN;
	% set to zero, the values of Sn that are less than or 
	% equal to tole
	toler = abs(Sn) <= tole;
	Sn(toler == 1) = 0;
	% check if a negative value exists in Sn or otherwise
	% continue with the next segment
	negativeFound = ~isempty(Sn < 0);
	currentSegment = currentSegment + 1;
end
end