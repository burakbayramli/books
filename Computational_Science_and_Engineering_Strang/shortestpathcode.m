%8.6  shortestpathcode.m

function [M] = all_pairs(A)
% function [M] = all_pairs(A)
% 
% A should be the adjacency matrix of a graph, possibly weighted.
% D is the induced distance between all pairs of nodes.
% If there is no path from node i to j, then D(i,j) will be inf.
%
% A is assumed to be a directed adjacency matrix, so if you desire
% the result for an undirected graph, be sure that A is symmetric.

% we now need to put infs on the non-edges.
% the following two lines would work if only inf*0 = 0.
%
% M0 = ((A+eye(length(A))) == 0);
% M = A + inf*M0
%
% instead, we use the following, which would be faster if A was
% in sparse form--i.e. an edge list:

M = inf*ones(length(A));
for i = 1:length(A),
  for j = 1:length(A)
    if (i == j | A(i,j) ~= 0)
      M(i,j) = A(i,j);
    end
  end
end


for i = 1:ceil(log2(length(A))),
  M = ms_mult(M,M);
end

-----

function [M] = ms_mult(A,B)
% function [M] = ms_mult(A,B)
%
% Min-Sum multiplication.
%
% This performs a matrix multiplication on A and B,
% but with product replaced by sum and sum replaced by min.
%
% The routine assumes that A and B have the same size, and are square.
%
% ms_mult is designed to work for directed graphs--to adjust it
% for symmetric graphs run j = (i+1):length(M)

M = zeros(size(A));

for i = 1:length(M),
  for j = 1:length(M),
    M(i,j) = min(A(i,:) + B(:,j)');
  end
end
