function [BasisInv] = pfi(BasisInv, h_l, r)
% Filename: pfi.m
% Description: the function is an implementation of the 
% Product Form of the Inverse basis update method
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [BasisInv] = pfi(BasisInv, h_l, r)
% 
% Input:
% -- BasisInv: matrix with the basis inverse (size m x m)
% -- h_l: vector with pivot column (size m x 1)
% -- r: the index of the leaving variable
%
% Output:
% -- BasisInv: matrix with the new basis inverse 
%    (size m x m)

eta = -h_l / h_l(r); % compute the eta column vector
eta(r) = 1 / h_l(r);
EInv = speye(length(BasisInv)); % create eta matrix
EInv(:, r) = eta;
BasisInv = EInv * BasisInv; % calculate new basis inverse
end