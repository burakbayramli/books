function [M, z] = normalize(A)
% normalize: Normalize a vector/matrix so it sums to 1
% M(i) = A(i)/sum(A)

z = sum(A(:));
% Set any zeros to one before dividing
% This is valid, since c=0 => all i. A(i)=0 => the answer should be 0/1=0
s = z + (z==0);
M = A / s;


