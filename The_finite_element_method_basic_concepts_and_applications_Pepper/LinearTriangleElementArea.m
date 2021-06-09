function y = LinearTriangleElementArea(xi,yi,xj,yj,xm,ym)
%LinearTriangleElementArea   This function returns the area of the
%                            linear triangular element whose first 
%                            node has coordinates (xi,yi), second 
%                            node has coordinates (xj,yj), and 
%                            third node has coordinates (xm,ym).
y = (xi*(yj-ym) + xj*(ym-yi) + xm*(yi-yj))/2;




