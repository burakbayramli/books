function [f,dfdt] = legsn(theta)
% legsn  Evaluate f(theta) and fprime(theta) for the picnic leg problem.
%        Used with the Newton's method.
%
% Synopsis:  [f,dfdt] = legsn(theta)
%
% Input:     theta = angle of table leg in radians
%            WLENGTH HLENGTH and BLENGTH are global variables giving
%                    table dimensions
%
% Output:    f    = value of function that, when theta is a root, is equal to zero
%            dfdt = fprime(theta), ie. df/dtheta
global  WLENGTH  HLENGTH  BLENGTH

f    = WLENGTH*sin(theta) - HLENGTH*cos(theta) - BLENGTH;
dfdt = WLENGTH*cos(theta) + HLENGTH*sin(theta);
