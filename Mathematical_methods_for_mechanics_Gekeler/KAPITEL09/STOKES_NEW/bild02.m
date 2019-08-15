function bild02
% Draws QUIVER for (U,V), CONTOUR for P
% Streamslice for (U,V), Streamline for (U,V)

load daten2 p e t  U V P
bilda = 100;
while ~ismember(bilda,[1,2,3,4])
   bilda = input(' Select figure! (1/2/3/4)  ');
end
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X)); P = P';
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[X1,Y1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,X1,Y1,'cubic');
t1      = t(1:3,:);
trimesh(t1',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis manual
switch bilda
case 1, disp(' flow ')
   quiver(X,Y,U.',V.',1), hold on
case 2, disp(' Contour for P ')
   W = griddata(X,Y,P,X1,Y1,'cubic');
   [C,h] = contour(X1,Y1,W,20,'k');
  % [C,h] = contour(X1,Y1,W,10);
  % clabel(C,h,'manual');
case 3, disp(' Streamslice for stream function ')
   U1 = griddata(X,Y,U,X1,Y1,'cubic');
   V1 = griddata(X,Y,V,X1,Y1,'cubic');
   streamslice(X1,Y1,U1,V1)
case 4
   U1 = griddata(X,Y,U,X1,Y1,'cubic');
   V1 = griddata(X,Y,V,X1,Y1,'cubic');
   XSTART  = [0.05,0.1,0.2,0.3,0.4];
   YSTART  = 0.5*ones(1,length(XSTART));
   H = streamline(X1,Y1,U1,V1,XSTART,YSTART,[0.1,500]);
end
clear
