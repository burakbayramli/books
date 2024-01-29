clear all;
load feT.dat;                 % load the results from the analysis
[x,y]=meshgrid(0:2/2:2,0:2/2:2);  % 2-D grid coordinates of the mesh
sx=x(1,1);     sy=y(1,1);     % lower-left corner of the domain
dx=x(1,2)-sx;  dy=y(2,1)-sy;  % size of the elements in x and y
T=x*0;                        % empty T matrix
n_nodes=size(feT,1);          % total number of nodes
% for-loop: set the temperature for each of the mesh grid points
for i=1:n_nodes
  xp=feT(i,2);
  yp=feT(i,3);
  col=round((xp-sx)/dx+1);
  row=round((yp-sy)/dy+1);
  T(row,col)=feT(i,4);
end
% surface plot of temperature
figure(1);
colormap(gray(256) );  % set up gray scale colormap
surf(x,y,T);
caxis([-50 150]);
set(gca,'fontsize',16);
xlabel('x');
ylabel('y');
zlabel('T(^oC)');
% contour plot of temperature
figure(2)
colormap(gray(256) );  % set up gray scale colormap
contourf(T);
set(gca,'fontsize',16);
xlabel('x');
ylabel('y');