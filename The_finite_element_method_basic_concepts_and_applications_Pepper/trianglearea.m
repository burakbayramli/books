function [ A ] = trianglearea( x1,y1,x2,y2,x3,y3 )
  A=(x1*y2-x2*y1)+(x3*y1-x1*y3)+(x2*y3-x3*y2);
  A=A/2
end
