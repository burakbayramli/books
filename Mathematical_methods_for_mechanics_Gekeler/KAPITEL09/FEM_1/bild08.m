function bild08
% Zeichnet MESH und CONTOUR fuer Z
% Rannacher-Turek-Elemente

load daten1 p e q p1 q1
load daten2 RD RC
load daten3 Z
bild = 100; KK = [1,2];
%while ~ismember(bild,KK)
%   bild  = input(' Welches Bild?, (1/2) ');
%end
bild = 1;
clf, hold on
mesh36(p,q,'b'), hold on
axis equal, grid on

X = p(1,:);  Y = p(2,:);
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(X,Y,Z,U,V);
switch bild
case 1
   %[C,h] = contour(U,V,W);
    [C,h] = contour(U,V,W,[4,6,8,10,12]);
   %clabel(C,h,'labelspacing',400),  hold on
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r'), hold on
   end
   %plot(X,Y,'.','MarkerSize',6);
case 2
   mesh(U,V,W)
end
clear
