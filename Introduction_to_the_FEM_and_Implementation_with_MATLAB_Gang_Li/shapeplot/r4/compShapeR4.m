
function [N]= compShapeR4(x0,y0,x1,y1,x,y)
  a=(x1-x0)/2;
  b=(y1-y0)/2;
  
  N(1,1)=(a+x)*(b+y)/4/a/b;
  N(2,1)=(a-x)*(b+y)/4/a/b;
  N(3,1)=(a-x)*(b-y)/4/a/b;
  N(4,1)=(a+x)*(b-y)/4/a/b;

