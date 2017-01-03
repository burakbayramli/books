function A=dag(pot)
%DAG Return the adjacency matrix (zeros on diagonal) for a Belief Newtork
% A=dag(pot)
%
% Assumes that pot(i) contains the distribution p(i|pa(i))
vars=[];
for p=1:length(pot)
    vars=[vars pot(p).variables];
end
N=max(vars); 
A=zeros(N,N);
for p=1:length(pot)
    A(pot(p).variables,p)=1;
end
A=A-diag(diag(A));