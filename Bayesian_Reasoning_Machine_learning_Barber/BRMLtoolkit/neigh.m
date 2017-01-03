function n=neigh(G,v,varargin)
%NEIGH Find the neighbours of vertex v on a graph with adjacency matrix G
% n=neigh(G,v,<'set' or 'union'>)
% By default returns the union of all neighbours for a vector of nodes v.
% If called with 'set', then n{i} contains the neighours of vertex v(i)

if nargin==2;neightype='union';else neightype=varargin{1}; end
switch neightype
    case {'union'}
        for i=1:length(v)
            a=find(G(:,v(i))+G(v(i),:)');
            n=setdiff(a(:)',v(i));
        end
    case {'set'}
        for i=1:length(v)
            [a b]= find(G(:,v(i))+G(v(i),:)');
            n{i}=setdiff([a' b'],v(i));
        end
end