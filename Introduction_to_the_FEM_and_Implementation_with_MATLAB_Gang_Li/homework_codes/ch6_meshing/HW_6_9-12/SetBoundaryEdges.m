function bpoints=SetBoundaryEdges(shape, bpos)
global nodes n_nodes edges;

n_shapes=shape(1,1);
n_interior_shapes=n_shapes-1;
bpoints=zeros(n_nodes,1);

n_exterior_boundary_nodes=shape(2,1);
for i=1:n_nodes
  bpoints(bpos(i,1),1)=i;
end

for i=1:n_exterior_boundary_nodes
  j=GetNext(1,n_exterior_boundary_nodes,i);
  edge=IsExistingEdge(bpoints(i),bpoints(j));
  edges(edge,3)=2;
end

s=i+1; e=s-1;
for k=1:n_interior_shapes
  n_interior_shape_nodes=shape(k+2,1);
  e=e+ n_interior_shape_nodes;
  for i=s:e
    j=GetNext(s,e,i);
    edge=IsExistingEdge(bpoints(i),bpoints(j));
    edges(edge,3)=2;
  end
  s=e+1;
end