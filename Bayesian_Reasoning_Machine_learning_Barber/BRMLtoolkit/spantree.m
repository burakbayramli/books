function [Atree elimseq weight]=spantree_test(edgelistORmatrix)
%SPANTREE Find a spanning tree from an edge list (Prim's algorithm)
% [Atree elimseq weight]=spantree(edgelistORmatrix)
% The spanning tree is returned as an adjacency matrix, along with an elimination sequence and weight of the tree.
%
% Input: One may call using an edgelist such as [ [1 2]; [1 4]; [2 3]]. 
%        Alternatively, one may call using an adjacency matrix with
%        weighted edges [ 0 2 0; 2 0 1; 0 1 0]
%
% Output:
% Atree : adjacency matrix of the spanning tree
% elimeq: node elimination sequence for the spanning tree
% weight: weight (sum of the edge weights) of the spanning tree.
%
% Note: to find a max weight spanning tree based based on an edgelist, call this routine with the edges ordered heaviest first.
s=size(edgelistORmatrix);
if s(1)==s(2)
    A=edgelistORmatrix; A=A-diag(diag(A));
    N=size(A,1);
    [edgelist edgeweights]=edges(A);
    [tmp ord]=sort(edgeweights,'descend');
    edgelist=edgelist(ord,:);   
else
    edgelist=edgelistORmatrix;
    N=max(edgelist(:)); % number of nodes in the graph
    A=sparse(N,N);
    M=size(edgelist,1);
    for ed=1:M; A(edgelist(ed,1),edgelist(ed,2))=M-ed;end % get the adj matrix
    A=A+A'; % make symmetric
end
v=sparse(N,1);v(1,1)=1; % check if connected:
for n=1:N; v=A*v; end; if ~all(v>0); warning('graph not connected');end
Atree=sparse(N,N); % adjacency matrix of the tree
[a b]=assign(edgelist(1,:)); Atree(a,b)=1; Atree(b,a)=1;% add first edge
weight=A(a,b); A(a,b)=0; A(b,a)=0;
includednodes=[a b];
while length(includednodes)<N
    candedges=edgeneigh(A,includednodes);
    % sort the edges according to weight:
    clear w; for ed=1:size(candedges,1); w(ed)=A(candedges(ed,1),candedges(ed,2)); end
    [val sed]=sort(w,'descend'); candedges=candedges(sed,:);
    for edge=1:length(candedges)
        if ~all(ismember(candedges(edge,:),includednodes)) % discard edges that would make a loop
            weight=weight+A(candedges(edge,1),candedges(edge,2));
            [dum ind]=find(~ismember(candedges(edge,:),includednodes));
            includednodes=[includednodes candedges(edge,ind)];
            A(candedges(edge,1),candedges(edge,2))=0; A(candedges(edge,2),candedges(edge,1))=0;
            Atree(candedges(edge,1),candedges(edge,2))=1; Atree(candedges(edge,2),candedges(edge,1))=1;
            break
        end
    end
end
elimseq=fliplr(includednodes);
if s(1)~=s(2); weight=N-1; end