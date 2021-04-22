function Rh=shift(Ah,x0,y0)
%inputs a 3 by n matrix of homogeneous vertex coordinates, and xy-coors.
%x0,y0 of shift vector.  Output is homogeneous coor. matrix of shifted
%vetices

%first construct shifting matrix
S=[1 0 x0;0 1 y0;0 0 1];
%now we can obtain the desired rotated vertices:
Rh=S*Ah;
