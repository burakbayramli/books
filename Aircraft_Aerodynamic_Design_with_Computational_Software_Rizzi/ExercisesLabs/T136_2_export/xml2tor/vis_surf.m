function stat = vis_surf(xyz1,xyz2)
% render lofted surface between 
% 3D polygons xyz1 and xyz2
[n1,dum] = size(xyz1);
[n2,dum] = size(xyz2);
n = 31;
s = [0;cumsum(sqrt(diff(xyz1(:,1)).^2+diff(xyz1(:,2)).^2+diff(xyz1(:,3)).^2))];
smax = max(s);
slist = linspace(0,smax,n);
x1 = interpspl(s,xyz1(:,1),slist);
y1 = interpspl(s,xyz1(:,2),slist);
z1 = interpspl(s,xyz1(:,3),slist);
s  = [0;cumsum(sqrt(diff(xyz2(:,1)).^2+diff(xyz2(:,2)).^2+diff(xyz2(:,3)).^2))];
smax  = max(s);
slist = linspace(0,smax,n);
x2 = interpspl(s,xyz2(:,1),slist);
y2 = interpspl(s,xyz2(:,2),slist);
z2 = interpspl(s,xyz2(:,3),slist);
X  = [x1,0.5*(x1+x2),x2];
Y  = [y1,0.5*(y1+y2),y2];
Z  = [z1,0.5*(z1+z2),z2];
light = [1;1;1];
surfl(X,Y,Z,light);
stat = 1;