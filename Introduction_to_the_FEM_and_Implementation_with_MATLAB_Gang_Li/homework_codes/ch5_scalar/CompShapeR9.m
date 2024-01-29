
function [N]= CompShapeR9(x0,y0,x1,y1,x,y)
  xc=(x1+x0)/2;
  yc=(y1+y0)/2;
  
  N(1,1)=(x-x0)*(x-xc)/((x1-x0)*(x1-xc)) * (y-y0)*(y-yc)/((y1-y0)*(y1-yc));
  N(2,1)=(x-x1)*(x-xc)/((x0-x1)*(x0-xc)) * (y-y0)*(y-yc)/((y1-y0)*(y1-yc));
  N(3,1)=(x-x1)*(x-xc)/((x0-x1)*(x0-xc)) * (y-y1)*(y-yc)/((y0-y1)*(y0-yc));
  N(4,1)=(x-x0)*(x-xc)/((x1-x0)*(x1-xc)) * (y-y1)*(y-yc)/((y0-y1)*(y0-yc));

  N(5,1)=(x-x1)*(x-x0)/((xc-x1)*(xc-x0)) * (y-y0)*(y-yc)/((y1-y0)*(y1-yc));
  N(6,1)=(x-x1)*(x-xc)/((x0-x1)*(x0-xc)) * (y-y0)*(y-y1)/((yc-y0)*(yc-y1));
  N(7,1)=(x-x1)*(x-x0)/((xc-x1)*(xc-x0)) * (y-y1)*(y-yc)/((y0-y1)*(y0-yc));
  N(8,1)=(x-x0)*(x-xc)/((x1-x0)*(x1-xc)) * (y-y0)*(y-y1)/((yc-y0)*(yc-y1));
  
  N(9,1)=(x-x1)*(x-x0)/((xc-x1)*(xc-x0)) * (y-y0)*(y-y1)/((yc-y0)*(yc-y1));
  