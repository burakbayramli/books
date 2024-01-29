function DeleteTriByEdge(eid)
global edges tris;
[egs,nds]=NeighborEdgesNodes(eid);  % get neighbor edges of eid
% for-loop: clean up neighbor edges' record of the triangles
for i=1:2 
  tri=edges(eid,8+i);
  if tri~=0
    for j=1:size(egs,1)
      for k=9:10
        if edges(egs(j),k)==tri; edges(egs(j),k)=0; end;
      end
    end
  end
end
% for-loop: clean up edge "eid" and delete the triangle    
for i=1:2
  tri=edges(eid,8+i);
  edges(eid,8+i)=0;
  if tri~=0; tris(tri,6)=0; end;
end