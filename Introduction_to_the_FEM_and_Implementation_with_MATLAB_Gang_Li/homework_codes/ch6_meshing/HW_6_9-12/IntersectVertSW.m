function re=IntersectVertSW(edge_in,ix,iy)
global edges nodes TOL;

x1=nodes(edges(edge_in,1),1);
y1=nodes(edges(edge_in,1),2);
x2=nodes(edges(edge_in,2),1);
y2=nodes(edges(edge_in,2),2);

if (x1<=ix-TOL && x2<=ix-TOL) || (x1>=ix+TOL && x2>=ix+TOL) 
  re=0; 
elseif abs(x1-ix)<TOL && abs(x2-ix)<TOL 
  if (iy-y1)*(iy-y2)<TOL, re=-1;       % on the edge=inside 
  else re=2;
  end
elseif (abs(x1-ix)<TOL && x2-ix>TOL) ||...
       (abs(x2-ix)<TOL && x1-ix>TOL)
  re=0;
else 
  oy=(ix-x1)/(x2-x1)*(y2-y1)+y1;
  if oy>iy+TOL, re=0;
  elseif oy>iy-TOL, re=-1;             % on the edge=inside 
  else re=1;
  end  
end