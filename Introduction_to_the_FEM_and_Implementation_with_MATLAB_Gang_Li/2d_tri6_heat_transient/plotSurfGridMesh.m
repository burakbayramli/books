clear all;

load x.dat;
load y.dat;
load feT.dat;

nx=size(x,2)-1;
ny=size(y,1)-1;
sx=x(1,1);
ex=x(1,nx+1);
sy=y(1,1);
ey=y(ny+1,1);

dx=(ex-sx)/nx;
dy=(ey-sy)/ny;

T=zeros(ny+1,nx+1);

n_nodes=size(feT,1);

for i=1:n_nodes
  xp=feT(i,2);
  yp=feT(i,3);
  col=(xp-sx)/dx+1;
  row=(yp-sy)/dy+1;
  T(row,col)=feT(i,4);
end

figure(1);

surf(x,y,T);

set(gca,'fontsize',16);
xlabel('x');
ylabel('y');
zlabel('T(^oC)');


figure(2)
%colormap( flipud(gray(256)) );
contourf(T);

set(gca,'fontsize',16);
xlabel('x');
ylabel('y');

print -depsc Tcontour.eps


    