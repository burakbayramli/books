function [ I ] = inv2D(A,detA)
%
% compute inverse of a 2x2 matrix
% given matrix A and its determinant
%
 I(1,1)  = +A(2,2)/detA;
 I(2,1)  = -A(2,1)/detA;
 I(1,2)  = -A(1,2)/detA;
 I(2,2)  = +A(1,1)/detA;
end