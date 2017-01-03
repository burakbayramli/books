function s = scale2(a);
% Synopsis: s = scale2(a).
% Finds a power-of-2 full scale for the vector a.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

s =  exp(log(2)*ceil(log(max(abs(a)))./log(2)));

