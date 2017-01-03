function [tree, elimseq, schedule]=istree(A,varargin)
%ISTREE Check if graph is singly-connected (a polytree)
% [tree, elimseq, schedule]=istree(A,<root>)
%
% Input : an adjacency matrix A (zeros on diagonal)
%
% Outputs:
% tree =1 if graph is singly connected, otherwise tree = 0
% elimseq is a variable elimination sequence in which simplical nodes of
% the tree are listed, as each simplical node is removed from the tree.
% schedule is the sequence of messages from node to node corresponding to elimseq
% If A is directed the elimination schedule begins with the nodes with no children
% If root is specified, the last node eliminated is root.

% If the graph is connected and the number of edges is less than the number of nodes, it must be a tree.
% However, to deal with the general case in which it is unknown if the graph is connected we check using elimination.
% A tree/singly-connected graph must admit a recursive simplical node elimination. That is at any stage
% in the elimination there must be a node with either zero or 1 neighbours in the remaining graph.
C = size(A,1); % number of nodes in the graph
schedule=zeros(C,2);
tree=1; % assume A is singly connected
AA=A; % adjacency of the eliminated graph
elimseq=[]; % set of variables eliminated (in sequence)
root=[]; if nargin==2; root=varargin{1}; end
for node=1:C
    % now find the number of neighbours:
    nn=(C+1)*ones(1,C);  % ensures that we don't pick eliminated nodes
    s=1:C; r=zeros(1,C); r(elimseq)=1; s=s(r==0);
    nn(s)=neighboursize(AA',s);
    if ~isempty(root)
        nn(root)=C+1; % ensures we don't pick root
    end
    [val elim]=min(nn); % find node with least number of neighbours
    neigh = find(AA(:,elim));
    if length(neigh)>1; tree=0; break; end % if least has more than 1 neighbour, cannot be a tree
    AA(elim,:)=0; AA(:,elim)=0; % eliminate node from graph
    elimseq=[elimseq elim]; % add eliminated node to elimination set
    if isempty(neigh);  schedule(node,:)=[elim elim];
    else
        schedule(node,:)=[elim neigh];
    end
end
if tree==0; elimseq=[]; end