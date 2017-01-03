function [cidx states]=count(data,nstates)
%COUNT for a data matrix (each column is a datapoint), return the state counts
% [cidx states]=count(data,nstates)
% data is a data matrix of samples of a set of variables. Variable i has
% states 1:nstates(i). cidx contains the counts of each of the states in the linear index.
% see also squeezestates.m
% example r=ceil(10*rand(1,20)); count(r,10)
if isempty(data); cidx=[]; states=[]; return; end
cidx=zeros(1,prod(nstates));
for n=1:size(data,2)
    ind = subv2ind(nstates,data(:,n)');
    cidx(ind)=cidx(ind)+1;
end
states=ind2subv(nstates,1:prod(nstates));