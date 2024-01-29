function re=IsInShape(edge_in)
global edges nodes last_edge_row;

x1=nodes(edges(edge_in,1),1);
y1=nodes(edges(edge_in,1),2);
x2=nodes(edges(edge_in,2),1);
y2=nodes(edges(edge_in,2),2);
x=(x1+x2)/2; y=(y1+y2)/2;
count=0;
for i=1:last_edge_row
  if edges(i,3)~=2, continue, end;
  count=count + IntersectVertSW(i,x,y);
  if mod(count,2)==0
    re=0;                              % if count is even: outside
  else 
    re=1;
  end
end