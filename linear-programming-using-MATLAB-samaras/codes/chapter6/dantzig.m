function [index] = dantzig(Sn)
% Filename: dantzig.m
% Description: the function is an implementation of the 
% Dantzig's pivoting rule
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [index] = dantzig(Sn)
% 
% Input:
% -- Sn: vector of reduced costs (size 1 x (n - m))
%
% Output:
% -- index: the index of the entering variable

[~, index] = min(Sn); % find the index of the variable 
% with the most negative Sn
end