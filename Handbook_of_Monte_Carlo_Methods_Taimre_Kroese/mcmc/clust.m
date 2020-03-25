function out= clust(c,i,A)
out = unique([c,i]);
for j=setdiff(find(A(i,:)),out)
    out = sort(unique(clust(out,j,A)));
end
