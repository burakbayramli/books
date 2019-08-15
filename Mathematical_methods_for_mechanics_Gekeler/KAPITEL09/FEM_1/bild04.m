function bild04
% Zeichnet MESH und CONTOUR fuer Z
% quadratische Dreieck- und Parallelogrammelemente

load daten1 p e t q p1 p2 t1 q1
load daten2 RD RC
load daten3 Z
bild = 100; KK = [1,2];
%while ~ismember(bild,KK)
%   bild  = input(' Welches Bild?, (1/2) ');
%end
bild = 1;
clf, hold on
X1 = p(1,:);  Y1 = p(2,:);
X2 = p1(1,:); Y2 = p1(2,:);
X  = [X1,X2]; Y  = [Y1,Y2];
if ~isempty(q)
   X3 = p2(1,:); Y3 = p2(2,:);
   X  = [X,X3]; Y = [Y,Y3];
end
Z1 = zeros(length(X1),1);
trimesh(t(1:3,:)',X1,Y1,Z1,'edgecolor','g'), hold on
mesh36(p,q,'g'), hold on
axis equal, grid on
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(X,Y,Z,U,V,'v4');
switch bild
case 1
   %contour(U,V,W), hold on
   [C,h] = contour(U,V,W,[4,6,8,10,12]);
   clabel(C,h,'labelspacing',400),  hold on
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r'), hold on
   end
   plot(X,Y,'.','MarkerSize',6);
case 2
   mesh(U,V,W)
end
axis equal
clear
