function AddTriangle(nids, eids)
global tris last_tri_row edges;
% next 6 lines: test if "tris" matrix is full, if Yes, double the size
row=last_tri_row+1;
if row>size(tris,1) 
  tmp=tris;
  tris=zeros(last_tri_row*2,6);
  tris(1:last_tri_row,:)=tmp;
end;

% add the triangle information to the last non-empty row of "tris"
tris(row,1:3)=nids';   tris(row,4:6)=eids';

% next 9 lines: update the "edges" matrix to record the added triangle
for i=4:6
  eid=tris(row,i);
  if edges(eid,9)==0; edges(eid,9)=row;
  elseif edges(eid,10)==0; edges(eid,10)=row;
  else fprintf('Add tris error\n');
  end
end

last_tri_row=last_tri_row+1; % the last non-empty row + 1