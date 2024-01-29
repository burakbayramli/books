
function PlotResultsSurfGrid(feT,sx,sy,ex,ey,nx,ny)
[x,y]=meshgrid(sx:(ex-sx)/nx:ex, sy:(ey-sy)/ny:ey);  % 2-D grid coordinates 
dx=(ex-sx)/nx;
dy=(ey-sy)/ny;
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

% next block: surface plot of temperature
figure(1);
clf;
colormap(gray(256) );  % set up gray scale colormap
surf(x,y,T);
caxis([-50 150]);
set(gca,'fontsize',16);
xlabel('x');
ylabel('y');
zlabel('T(^oC)');
