%2.9  fiedlercode.m

N=10; W=zeros(2*N,2*N);          % Generate 2N nodes, two clusters 
rand('state',100);               % Rand repeats to give same graph
for i=1:2*N-1
    for j=i+1:2*N
        p=0.7 - 0.6*mod(j-i,2);  % p=0.1 when j-i is odd, 0.7 else
        W(i,j)=rand < p;         % Insert edges with probability p
    end                          % All weights in W are 1 so G=A'A
end
W=W+W'; D=diag(sum(W));          % Adjacency matrix W and degree D
G=D-W; [V,E]=eig(G,D);           % Eigenvalues of Gx = (lambda) Dx
[a,b]=sort(diag(E));             % Eigenvalues in increasing order
z=V(:,b(2));  
