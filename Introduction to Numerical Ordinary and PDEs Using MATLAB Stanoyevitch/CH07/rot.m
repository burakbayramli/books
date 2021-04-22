function Rh=rot(Ah,x0,y0,theta)
%viz. EFR 7.6; theta should be in radians
%inputs a 3 by n matrix of homogeneous vertex coordinates, xy coordinates
%of a point and an angle theta.  Output is corresponding matrix of 
%vertices rotated by angle theta about (x0,y0).

%first construct homogeneous coordinate matrix for shifting (x0,y0) to (0,0)
SZ=[1 0 -x0;0 1 -y0; 0 0 1];
%next the rotation matrix at (0,0)
R=[cos(theta) -sin(theta) 0; sin(theta) cos(theta) 0;0 0 1];
%finally the shift back to (x0,y0)
SB=[1 0 x0;0 1 y0;0 0 1];
%now we can obtain the desired rotated vertices:
Rh=SB*R*SZ*Ah;
