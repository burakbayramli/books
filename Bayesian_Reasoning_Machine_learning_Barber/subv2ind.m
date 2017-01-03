function ndx = subv2ind(siz,sub)
% ndx = subv2ind(siz,sub)
%
% state to index : return the linear index of a state vector sub based on an array of size siz
% If sub is a matrix, each row is taken as a state vector and the linear index returned in the
% corresponding row of ndx. 
% This function is the inverse of ind2subv.m
k = [1 cumprod(siz(1:end-1))]; ndx=sub*k'-sum(k)+1;