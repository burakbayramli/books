function [nc,C] = mkclust(B)
%given the auxiliary variables {B_ij} organized in the symmetric 
%matrix B, this function clusters the sites;
%output:
% nc - number of clusters
% C  - gives the indexes of sites belonging to each cluster,
%       separated by zeroes;
n = size(B,1);
S = 1:n;
nc = 0;
C = [];
while ~isempty(S)
    i = min(S);
    c = clust([i],i,B);
    S = setdiff(S,c);
    C = [C,c,0];
    nc = nc+1;
end
