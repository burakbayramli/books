function y = SpaceFrameElementLength(x1,y1,z1,x2,y2,z2)
%SpaceFrameElementLength   This function returns the length of the
%                          space frame element whose first node has  
%                          coordinates (x1,y1,z1) and second node has  
%                          coordinates (x2,y2,z2).   
y = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1));





