function [elimseq]=elimtri(A,Vend)
%ELIMTRI Return a variable elimination sequence for a triangulated graph
% elimseq=elimtri(A,Vend)
%
% For an undirected triangulated graph with symmetric adjacency matrix A, return a
% variable elimination, with Vend being the last node/vertex in the
% sequence. The elimination proceeds by recursively removing nodes with the
% least neighbours.

A=A-diag(diag(A));
C = size(A,1); % number of nodes in the graph
schedule=zeros(C,2);
tree=1; % assume A is singly connected
AA=A; % adjacency of the eliminated graph
elimseq=[]; % set of variables eliminated (in sequence)
for node=1:C
    % now find the number of neighbours:
    nn=(C+1)*ones(1,C);  % ensures that we don't pick eliminated nodes
    s = setdiff(1:C,elimseq);  nn(s) = sum(AA(s,s));
    [val elim]=min(nn); % find node with least number of neighbours
    if elim==Vend
        [vals elims]=sort(nn); % find node with least number of neighbours
        elim=elims(2);
    end
    neigh = find(AA(elim,:));
    AA(elim,:)=0; AA(:,elim)=0; % eliminate node from graph
    elimseq=[elimseq elim]; % add eliminated node to elimination set
end
elimseq(end)=Vend;