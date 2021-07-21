function [difference, ratio] = Leibschnizel(N)
% Leibschnizel(N)
%
% Computes the difference and ratio of a = 1 + 2 + 3 + .. + N and b = N^2 / 2
a = sum(1:N);
b = N^2 / 2;
difference = a - b;
ratio = a / b;
