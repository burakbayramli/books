function [x1,y1,z1] = tr3(m,x,y,z)
%
%  For transforming multiple points according to the
%   linear transformation with matrix m
%
x1 = m(1,1) .* x +  m(1,2) .* y +  m(1,3) .* z ;
y1 = m(2,1) .* x +  m(2,2) .* y +  m(2,3) .* z ;
z1 = m(3,1) .* x +  m(3,2) .* y +  m(3,3) .* z ;
