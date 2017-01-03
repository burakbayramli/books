function y = sincos(x)
% sincos  Evaluates sin(x)*cos(x) for any input x
%
% Synopsis:  y = sincos(x)
%
% Input:     x = angle in radians, or vector of angles in radians
%
% Output:    y = value of sin(x)*cos(x) for each element in x

y = sin(x).*cos(x);
