function A=edges2adj(edgelist,N)
A=sparse(N,N);
for i=1:size(edgelist,1)
    A(edgelist(i,1),edgelist(i,2))=1;
end