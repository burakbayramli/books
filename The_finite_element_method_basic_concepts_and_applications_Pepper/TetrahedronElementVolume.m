function y = TetrahedronElementVolume(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
%TetrahedronElementVolume   This function returns the volume
%                           of the linear tetrahedral element
%                           whose first node has coordinates
%                           (x1,y1,z1), second node has 
%                           coordinates (x2,y2,z2), third node 
%                           has coordinates (x3,y3,z3), and 
%                           fourth node has coordiantes
%                           (x4,y4,z4).
xyz = [1 x1 y1 z1 ; 1 x2 y2 z2 ; 1 x3 y3 z3 ; 1 x4 y4 z4];
y = det(xyz)/6;




