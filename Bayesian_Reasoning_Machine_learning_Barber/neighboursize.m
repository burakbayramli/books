function nsize=neighboursize(A,varargin)
%NEIGHBOURSIZE number of neighbours in an undirected graph
%nsize=neighboursize(A,<node>)
% if node is missing return the neighbour sizes (including self) of each node
% If A is directed, returns the number of parents of the specified nodes
if isempty(varargin)
    nsize=sum(A,1);
else
    nodes=varargin{1};
    nsize=sum(A(:,nodes),1);
end