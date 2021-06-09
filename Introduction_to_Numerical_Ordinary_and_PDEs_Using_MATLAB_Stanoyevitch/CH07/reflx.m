function Rh=reflx(Ah,x0)
%inputs a 3 by n matrix of homogeneous vertex coordinates, x-coordinate
%x0 of reflection line. Output is corresponding matrix of 
%vertices reflected about line x=x0

%first construct homogeneous coordinate matrix for shifting x=x0 to x=0
SZ=[1 0 -x0;0 1 0; 0 0 1];
%next the reflection matrix at (0,0)
R=[-1 0 0; 0 1 0;0 0 1];
%finally the shift back 
SB=[1 0 x0;0 1 0;0 0 1];
%now we can obtain the desired rotated vertices:
Rh=SB*R*SZ*Ah;
