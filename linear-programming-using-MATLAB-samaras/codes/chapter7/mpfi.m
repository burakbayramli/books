function [BasisInv] = mpfi(BasisInv, h_l, r)
% Filename: mpfi.m
% Description: the function is an implementation of the 
% Modification of the Product Form of the Inverse basis 
% update method
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [BasisInv] = mpfi(BasisInv, h_l, r)
% 
% Input:
% -- BasisInv: matrix with the basis inverse (size m x m)
% -- h_l: vector with pivot column (size m x 1)
% -- r: the index of the leaving variable
%
% Output:
% -- BasisInv: matrix with the new basis inverse (size m x m)

eta = -h_l / h_l(r); % compute the eta column vector
eta(r) = 1 / h_l(r);
K = eta * BasisInv(r, :); % perform a simple outer product
BasisInv(r, :) = 0; % set equal to zero the elements in the 
% row r of the basis inverse
BasisInv = BasisInv + K; % add the matrices
end