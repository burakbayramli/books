function bild02
% Zeichnet MESH und CONTOUR fuer Z
% FORM = 1/2/3: Dreiecke/Parallelogramme/Vierecke

disp(' FORM festlegen: Dreiecke/Parallelogramme/Vierecke ')
FORM = 3;
load daten1 p e t
load daten2 RD RC
load daten3 Z
%bild = 100; KK = [1,2];
%while ~ismember(bild,KK)
%   bild  = input(' Welches Bild?, (1/2) ');
%end
bild = 1;
clf, hold on
X = p(1,:); Y = p(2,:);
Z1    = zeros(length(X),1);
if FORM == 1
   trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
end
if FORM == 2 | FORM == 3
   mesh36(p,t,'g'), hold on
end
axis equal, grid on
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(X,Y,Z,U,V,'v4');
switch bild
case 1
   contour(U,V,W,10);
  %[C,h] = contour(U,V,W,10);
  %clabel(C,h,'manual');
   hold on
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r');
      hold on
   end
   plot(X,Y,'.','MarkerSize',6);
case 2
   mesh(U,V,W)
end
clear
