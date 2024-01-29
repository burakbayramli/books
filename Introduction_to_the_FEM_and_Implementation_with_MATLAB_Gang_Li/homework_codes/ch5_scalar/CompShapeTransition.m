% shape functions of the transition element in Problem 5.3
function [N]= CompShapeTransition(x,y)
  
N(5,1)=(x+1)*(x+1/3)*(x-1)*(y+1)/(-32/27);
N(6,1)=(x+1)*(x-1/3)*(x-1)*(y+1)/(32/27);
N(7,1)=(y-1)*(y+1)*(x+1)/(-2);
N(1,1)=(x+1)*(y+1)/4 - 2/3*N(5,1) - 1/3*N(6,1);
N(2,1)=(x-1)*(y+1)/(-4) -2/3*N(6,1) - 1/3*N(5,1);
N(3,1)=(x-1)*(y-1)/4;
N(4,1)=(x+1)*(y-1)/(-4) - 1/2*N(7,1);  