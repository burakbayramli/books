function GpDAG=PCorient(G,S)
%PCOrient Partially orient the skeleton (using the Unmarried Collider rule only)
% GpDAG=PCorient(G,S)
GpDAG=G; V=size(G,1);
for x=1:V
    for y=x+1:V
        undirected_neighbours_x = intersect(find(GpDAG(x,:)),find(GpDAG(:,x)));
        undirected_neighbours_y = intersect(find(GpDAG(y,:)),find(GpDAG(:,y)));
        cands = intersect(undirected_neighbours_x,undirected_neighbours_y);
        for z=cands
            if ~ismember(z,S{x,y})
                GpDAG(z,x)=0; GpDAG(z,y)=0;
            end
        end
    end
end