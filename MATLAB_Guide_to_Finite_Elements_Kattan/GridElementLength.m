function y = GridElementLength(x1,y1,x2,y2)
%GridElementLength   This function returns the length of the
%                    grid element whose first node has  
%                    coordinates (x1,y1) and second node has  
%                    coordinates (x2,y2).   
y = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));




