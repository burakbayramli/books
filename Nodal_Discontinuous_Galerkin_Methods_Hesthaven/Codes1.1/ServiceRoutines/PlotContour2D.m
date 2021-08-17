function PlotContour2D(tri, x, y, u, levels)

% function PlotContour2D(tri, x, y, u, levels)
% Purpose: generic routine to plot contours for triangulated data
  
Nlevels = length(levels);

hold on;

v1 = tri(:,1); v2 = tri(:,2); v3 = tri(:,3);
u1 = u(v1);    u2 = u(v2);    u3 = u(v3);
x1 = x(v1);    x2 = x(v2);    x3 = x(v3);
y1 = y(v1);    y2 = y(v2);    y3 = y(v3);

allx = []; ally = [];
for n=1:Nlevels
  lev = levels(n);

  flag1 = max(u1,u2)>=lev & min(u1,u2)<=lev;   % edge 1
  flag2 = max(u2,u3)>=lev & min(u2,u3)<=lev;   % edge 2
  flag3 = max(u1,u3)>=lev & min(u1,u3)<=lev;   % edge 3
  
  c1 = (lev-u1)./(u2-u1);  xc1 = (1-c1).*x1 + c1.*x2; yc1 = (1-c1).*y1 + c1.*y2;
  c2 = (lev-u2)./(u3-u2);  xc2 = (1-c2).*x2 + c2.*x3; yc2 = (1-c2).*y2 + c2.*y3;
  c3 = (lev-u1)./(u3-u1);  xc3 = (1-c3).*x1 + c3.*x3; yc3 = (1-c3).*y1 + c3.*y3;
  
  ids = find(flag1+flag2==2);
  allx = [allx, [xc1(ids),xc2(ids)]'];
  ally = [ally, [yc1(ids),yc2(ids)]'];

  ids = find(flag2+flag3==2);
  allx = [allx, [xc2(ids),xc3(ids)]'];
  ally = [ally, [yc2(ids),yc3(ids)]'];

  ids = find(flag1+flag3==2);
  allx = [allx, [xc1(ids),xc3(ids)]'];
  ally = [ally, [yc1(ids),yc3(ids)]'];
end
hold off

ha = line(allx, ally);
set(ha, 'Color', 'red')

hold off;
return;
