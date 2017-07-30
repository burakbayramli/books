function [ det ] = det2D(A)
%
% compute determinant of a 2x2 matrix
%

det =  A(1,1)*A(2,2) - A(1,2)*A(2,1);