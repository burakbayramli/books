function [index] = partialPricing(Sn, segmentSize)
% Filename: partialPricing.m
% Description: the function is an implementation of the 
% static partial pricing method using Dantzig's pivoting 
% rule
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [index] = partialPricing(Sn)
% 
% Input:
% -- Sn: vector of reduced costs (size 1 x (n - m))
% -- segmentSize: the size of the segment
%
% Output:
% -- index: the index of the entering variable

currentSegment = 1; % initialize current segment
negativeFound = 0;
partialSn = [];
% stop when a negative value is found in Sn
while negativeFound == 0
	% the start of the segment
	Nstart = (currentSegment - 1) * segmentSize + 1;
	% the end of the segment
	Nend = Nstart + segmentSize - 1;
	% if the end is greater than the length of Sn, set 
	% Nend = length(Sn)
	if Nend > length(Sn)
        Nend = length(Sn);
    end
	% check if a negative value exists in that segment 
	% or otherwise continue with the next segment
	partialSn = Sn(Nstart:Nend);
	negativeFound = ~isempty(find(partialSn < 0));
	if negativeFound == 1
        % find the index of the variable with the most 
        % negative Sn	
        [~, index] = min(partialSn); 
        % find the position in the original vector Sn
        index = index + Nstart - 1;
	else % next segment
        currentSegment = currentSegment + 1;
	end
end
end