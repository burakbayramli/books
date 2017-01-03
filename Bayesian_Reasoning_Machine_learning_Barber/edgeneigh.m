function edgelist=edgeneigh(G,v,varargin)
%EDGENEIGH Find the edge neighbours of vertex v on a graph with adjacency matrix G
% edgelist=edgeneigh(G,v,<'set' or 'union'>)
% By default returns the edgelist of neighbours for a vector of nodes v.
% If called with 'set', then n{i} contains the edgelist of vertex v(i)
G=G-diag(diag(G));
if nargin==2;neightype='union';else neightype=varargin{1}; end
edgelist=[];
switch neightype
    case {'union'}
        [a edgelist(:,2)]=find(G(v,:)+G(:,v)');
        edgelist(:,1)=v(a);
    case {'set'}
        for i=1:length(v)
            [a edgelist{i}(:,2)]=find(G(v(i),:)+G(:,v(i))');
            edgelist{i}(:,1)=v(i);
        end
end