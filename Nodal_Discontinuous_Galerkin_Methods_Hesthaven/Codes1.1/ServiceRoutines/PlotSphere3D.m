function PlotSphere3D(umP)

% function PlotSphere3D(umP)
% Purpose: render warp and blend nodes

[X,Y,Z] = Nodes3D(umP);

% plot spheres at X,Y
[X,Y,Z] = xyztorst(X,Y,Z);
L1 =  0.5*(1+Y); L2 = -0.5*(1+X+Y+Z);
L3 =  0.5*(1+X); L4 =  0.5*(1+Z);

min(L1),max(L1)
min(L2),max(L2)
min(L3),max(L3)
min(L4),max(L4)

tol = 1e-10;
idsI = find(L1>tol & L2 > tol & L3 > tol & L4 > tol);
ids1 = find(L1<tol);
ids2 = find(L2<tol);
ids3 = find(L3<tol);
ids4 = find(L4<tol);
idsV = find(L1+L2+L3<tol | L1+L2+L4<tol | L1+L3+L4< tol | L2+L3+L4<tol);

flag =  zeros(size(X));
flag(idsI) = 0;
flag(ids1) = 1;
flag(ids2) = 2;
flag(ids3) = 3;
flag(ids4) = 4;

idsE = find(( (L1<tol) + (L2<tol) + (L3<tol) + (L4<tol) )>=2);
flag(idsE) = 5;

flag(idsV) = 6;

v1 = [-1;-1/sqrt(3);-1/sqrt(6)];
v2 = [ 1;-1/sqrt(3);-1/sqrt(6)];
v3 = [ 0; 2/sqrt(3);-1/sqrt(6)];
v4 = [ 0; 0/sqrt(3); 3/sqrt(6)];

sX =   L3*v1(1) + L4*v2(1) + L2*v3(1) + L1*v4(1);
sY =   L3*v1(2) + L4*v2(2) + L2*v3(2) + L1*v4(2);
sZ =   L3*v1(3) + L4*v2(3) + L2*v3(3) + L1*v4(3);

f1 = [v1,v2,v3,v1];
f2 = [v1,v2,v4,v1];
f3 = [v2,v3,v4,v2];
f4 = [v3,v1,v4,v3];
plot3(f1(1,:),f1(2,:),f1(3,:), 'k-', 'LineWidth', 2) 

hold on;
plot3(f2(1,:),f2(2,:),f2(3,:), 'k-', 'LineWidth', 2) 
plot3(f3(1,:),f3(2,:),f3(3,:), 'k-', 'LineWidth', 2) 
plot3(f4(1,:),f4(2,:),f4(3,:), 'k-', 'LineWidth', 2) 

[xs,ys,zs] = sphere(80);
cs = 0*xs;
%ra= 0.05;
ra = 0.075;
if(0)
col(1,:) =   [.9 .9 0];
col(2,:) =   [.9 0 0];
col(3,:) =   [0 .9 0];
col(4,:) =   [0 0 .9];
col(5,:) =   [.9 0 .9];
col(6,:) = [.9, .9, .9];
col(7,:) = [0 0 0]; 
else
col(1,:) = [.9 .9 .9];
col(2,:) = [.8 .8 .8];
col(3,:) = [.7 .7 .7];
col(4,:) = [.6 .6 .6];
col(5,:) = [.5 .5 .5];
col(6,:) = [.4 .4 .4];
col(7,:) = [0  0   0];

end
colormap(col)

flag

for n=1:length(X)
  ha = surf(ra*xs+sX(n), ra*ys+sY(n), ra*zs + sZ(n), flag(n)+1 + cs);
  
  shading interp
  material shiny
end
lighting gouraud
camlight 

hold off; axis off; axis equal

view([0.8601    0.7398         0   -0.6687;
   -0.1468    0.1307    0.9848   -0.4027;
   -0.8323    0.7414   -0.1736    9.5041;
         0         0         0    1.0000]);

return;
