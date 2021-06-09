function f = legs(theta)
% legs  Evaluate f(theta) for picnic leg geometry.  Used with root-finders.
%
% Synopsis:  f = legs(theta)
%
% Input:     theta = angle of table leg in radians
%            global variables WLENGTH HLENGTH and BLENGTH specify the
%                   overall dimensions of the table
%
% Output:    f = value of function that, when theta is a root, is equal to zero

global  WLENGTH  HLENGTH  BLENGTH

f = WLENGTH*sin(theta) - HLENGTH*cos(theta) - BLENGTH;
