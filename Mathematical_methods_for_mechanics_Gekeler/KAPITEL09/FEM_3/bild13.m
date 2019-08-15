function bild12
load daten12 p e t UXX UYY VXX VYY UXXE UYYE VXXE VYYE U UE V VE
load daten12a CENTERS UXXA

bilda = 100;
%while ~ismember(bilda,[1,2])
%   bilda = input(' Which figure?, (1/2/3/4/5/6/7/8) ');
%end
bilda = 8;
% -- Eckpunkte
clf, hold on % fuer flaches Bild
plot(-0.1,-0.1,'w.'), hold on
plot(1.1,1.1,'w.'), hold on
axis equal tight, axis  manual, grid on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),40);
ylin    = linspace(min(Y),max(Y),40);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
% -- Rand -------------------------
RANDX = [p(1,e(1,:)),p(1,e(1,1))];
RANDY = [p(2,e(1,:)),p(2,e(1,1))];
plot(RANDX,RANDY,'r','linewidth',2,'erasemode','none'), hold on
switch bilda
case 8
   clf
   W1 = griddata(X,Y,UXXE,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.3,1,0.5,0,-0.5,-1,-1.3],'k'); hold on
   %clabel(C,h,'labelspacing',1000)
   W1 = griddata(X,Y,UXXA,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.3,1,0.5,0,-0.5,-1,-1.3],'r');
   %   [C,h] = contour(U1,V1,W1,10,'r');

   %clabel(C,h,'labelspacing',800)
   %mesh(U1,V1,W1)
   %axis tight




%   clf
%   W1 = griddata(X,Y,UXXE,U1,V1,'cubic');
%   [C,h] = contour(U1,V1,W1,[1.3,1,0.5,0,-0.5,-1,-1.3],'k'); hold on
%   %clabel(C,h,'labelspacing',1000)
%   CX = CENTERS(1,:); CY = CENTERS(2,:);
%   %xlin    = linspace(min(CX),max(CX),40);
%   %ylin    = linspace(min(CY),max(CY),40);
   %[U1,V1] = meshgrid(xlin,ylin);

 %  W1 = griddata(X,Y,UXXA,U1,V1,'cubic');
 %  [C,h] = contour(X,Y,UXXA,[1.3,1,0.5,0,-0.5,-1,-1.3],'r');
 %  [C,h] = contour(CX,CY,UXXA,5,'r');

  % clabel(C,h,'labelspacing',800)
end
