% uses the midpoint rule to estimate f'(x) 
%
% x is the point the derivative is being approximated at, h is the 
% interval distance the approximation is over.  Requires suboroutine
% f.m
%
% function y = d_mid(@f,x,h)
%
function y = d_mid(f,x,h)

y = (f(x+h)-f(x-h))/(2*h);
