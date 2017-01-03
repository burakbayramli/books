function qdplot(u,scale)
%QDPLOT Quadrilateral displacement plot for square model problem.
%   QDPLOT(U) plots the deformed mesh for the displacement field in U.
%   U should be a vector of length 2*(N+1)^2, where N+1 is the number of
%   node points along a side of the square. The field is scaled to make
%   the largest displacement 10% of the size of the square.
%
%   QDPLOT(U,SCALE) turns off the automatic scaling and multiplies all
%   displacements by SCALE.
%
%   Example:
%      [x,y]=ndgrid(0:0.05:1,0:0.05:1);
%      U=[y(:).^2;sin(2*pi*x(:))];
%      qdplot(U)
%
%   See also: ASSEMBLE, MKMODEL.

%   Per-Olof Persson <persson@math.mit.edu>

n=sqrt(length(u)/2)-1;
u1=reshape(u(1:(n+1)^2),n+1,n+1);
u2=reshape(u((n+1)^2+1:2*(n+1)^2),n+1,n+1);
um=sqrt(u1.^2+u2.^2);

if nargin<2
  dmax=max(um(:));
  scale=0.1/dmax;
end

[x,y]=ndgrid((0:n)/n,(0:n)/n);
xx=x+u1*scale;
yy=y+u2*scale;
x1=[xx(1:n,1:n),xx(2:n+1,1:n),xx(2:n+1,2:n+1),xx(1:n,2:n+1)];
y1=[yy(1:n,1:n),yy(2:n+1,1:n),yy(2:n+1,2:n+1),yy(1:n,2:n+1)];
cu=(um(1:n,1:n)+um(2:n+1,1:n)+um(1:n,2:n+1)+um(2:n+1,2:n+1))/4;

clf
patch(reshape(x1,n^2,4)',reshape(y1,n^2,4)',cu(:)')
line([1,0,0,1,1],[1,1,0,0,1],'color','k','linestyle','--','linewidth',2)
set(gcf,'renderer','zbuffer');
view(2)
axis equal
axis off
colorbar
