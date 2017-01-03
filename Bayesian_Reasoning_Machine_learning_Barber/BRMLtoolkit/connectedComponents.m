function cluster=connectedComponents(A)
%CONNECTEDCOMPONENTS Find the connected components an undirected graph 
% cluster=connectedComponents(A)
% for a symmetric adjacency matrix A return the component membership of each node 
A=sparse(A); N=size(A,1); A = A+A';
connected=(A+speye(N,N))^N; % each node is connected to itself
cl=0; 
for k=1:N
    vars=find(connected(:,k));
    if ~isempty(vars);
        cl=cl+1; % need another component
        cluster(vars)=cl;
        connected(vars,vars)=0;
    end
end