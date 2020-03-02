function [index] = partialPricingInitial(Sn)
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
%
% Output:
% -- index: the index of the entering variable

% find the index of the variable with the most 
% negative Sn
[~, index] = min(Sn);
end