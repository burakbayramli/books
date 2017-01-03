function [e weight]=edges(A)
%EDGES Return edge list (excluding self edges) from adjacency matrix A and associated non-zero edge weights
% [e weight]=edges(A)
A=A-diag(diag(A));
[edgea edgeb]=find(A); e=[edgea edgeb];weight=A(A~=0);