function y = QuadTriangleElementArea(x1,y1,x2,y2,x3,y3)
%QuadTriangleElementArea   This function returns the area of the
%                          quadratic triangular element whose first 
%                          node has coordinates (x1,y1), second 
%                          node has coordinates (x2,y2), and 
%                          third node has coordinates (x3,y3).
y = (x1*(y2-y3) + x2*(y3-y1) + x3*(y1-y2))/2;




