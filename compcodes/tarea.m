
%                 Function tArea 
%  This function calculates the area of a surface consisting of triangles
%  over a mesh with spacing dx and dy. The call is area(z,dx,dy,Z).
%  The matrix Z, (m+1) by (n+1), contains the heights of the surface
%  over the meshpoints.  The vector z contains all the "interior" 
%  elements of Z,arranged  as a row vector,
%  Z(i,j), 2 <= i <= m-1 and 2 <= j <= n-1.
%  The resulting function of z can be minimized by fmins.

function A = tarea(z,dx,dy,Z)
m = size(Z,1)-1;
n = size(Z,2)-1;
nn = n-1;
for i = 2:m
   Z(i, 2:n) = z((i-2)*nn +1: (i-1)*nn); 
end

  Z1 = diff(Z);
  Zvert = Z1(:, 1:n);
  Z2 = (diff(Z'))';
  Zhort = Z2(1:m, :);

  dx2 = dx^2; dy2 = dy^2;

  dAlower = .5*sqrt(dx2*dy2 + dx2*Zvert.^2 + dy2*Zhort.^2);
  Alower = sum(sum(dAlower));


  Zvert = Z1(:,2:n+1);
  Zhort = Z2(2:m+1,:);
  dAupper = .5*sqrt(dx2*dy2 + dx2*Zvert.^2 + dy2*Zhort.^2);
  Aupper = sum(sum(dAupper));

  A = Alower + Aupper;
 
