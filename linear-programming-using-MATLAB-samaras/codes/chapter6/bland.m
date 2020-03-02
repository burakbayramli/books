function [index] = bland(Sn, NonBasicList)
% Filename: bland.m
% Description: the function is an implementation of the 
% Bland's pivoting rule
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [index] = bland(Sn, NonBasicList)
% 
% Input:
% -- Sn: vector of reduced costs (size 1 x (n - m))
% -- NonBasicList: vector of indices of the nonbasic 
%    variables (size 1 x (n - m)) 
%
% Output:
% -- index: the index of the entering variable

temp = find(Sn < 0); % find the indices of the eligible 
% variables that can enter the basis
[~, b] = min(NonBasicList(temp)); % find the leftmost among 
% the eligible variables
% calculate the index of the entering variable
index = temp(b);
end