function v=validgridposition(x,y,Gx,Gy)
%VALIDGRIDPOSITION Returns 1 if point is on a defined grid
% v=validgridposition(x,y,Gx,Gy)
% returns 1 if (x,y) is on the grid (1:Gx,1:Gy)
v=1;
if x>Gx | x<1
	v=0;
end
if y>Gy | y<1
	v=0;
end