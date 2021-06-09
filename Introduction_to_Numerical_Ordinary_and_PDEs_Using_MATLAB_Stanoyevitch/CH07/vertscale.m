function Rh=vertscale(Ah,b,y0)
%inputs a 3 by n matrix of homogeneous vertex coordinates, a (pos.)
%numbers a for  y- scales, and an optional  arguments y0
%for center of scaling.  Output is homogeneous coor. matrix of scaled
%vertices.  default value of y0 is 0.

if nargin <3
    y0=0;
end
%first construct homogeneous coordinate matrix for shifting y=y0 to y=0
SZ=[1 0 0;0 1 -y0; 0 0 1];
%next the scaling matrix at (0,0)
S=[1 0 0; 0 b 0;0 0 1];
%finally the shift back to y=0
SB=[1 0 0;0 1 y0;0 0 1];
%now we can obtain the desired scaled vertices:
Rh=SB*S*SZ*Ah;
