function [f,dfdt] = legsnNG(theta,w,h,b)
% legsnNG  Evaluate f(theta) and fprime(theta) for the picnic leg problem.
%          Used with the Newton's method.
%
% Synopsis:  [f,dfdt] = legsnNG(theta)
%
% Input:     theta = angle of table leg in radians
%            w,h,b = table dimensions
%
% Output:    f    = value of function that, when theta is a root, is equal to zero
%            dfdt = fprime(theta), ie. df/dtheta
f    = w*sin(theta) - h*cos(theta) - b;
dfdt = w*cos(theta) + h*sin(theta);
