function bild12
load daten12 p e t UXX UYY VXX VYY UXXE UYYE VXXE VYYE U UE V VE
load daten12a CENTERS UXXB

bilda = 100;
while ~ismember(bilda,[1,2,3,4,5,6,7,8])
   bilda = input(' Which figure?, (1/2/3/4/5/6/7/8) ');
end
%bilda = 4;
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
case 1, disp (' u_xx exact and numerical ') 
   W1 = griddata(X,Y,UXXE,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.3,1,0.5,0,-0.5,-1,-1.3],'k');
    clabel(C,h,'labelspacing',1000)
   W1 = griddata(X,Y,UXX,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.3,1,0.5,0,-0.5,-1,-1.3],'r');
   clabel(C,h,'labelspacing',800)
 
case 2, disp (' u_yy exact and numerical ') 
   W1 = griddata(X,Y,UYYE,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.5,1,0.5,0,-0.5,-1,-1.5],'k');
 %   clabel(C,h,'labelspacing',1000)
   W1 = griddata(X,Y,UYY,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.5,1,0.5,0,-0.5,-1,-1.5],'r');
 %  clabel(C,h,'labelspacing',1000)
 
case 3, disp (' v_xx exact and numerical ') 
   W1 = griddata(X,Y,VXXE,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.5,1,0.5,0,-0.5,-1,-1.5],'k');
 %   clabel(C,h,'labelspacing',1000)
   W1 = griddata(X,Y,VXX,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.5,1,0.5,0,-0.5,-1,-1.5],'r');
   clabel(C,h,'labelspacing',1000)
 
case 4, disp (' v_yy exact and numerical ') 
   W1 = griddata(X,Y,VYYE,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.5,1,0.5,0,-0.5,-1,-1.5],'k');
 %   clabel(C,h,'labelspacing',1000)
   W1 = griddata(X,Y,VYY,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[1.5,1,0.5,0,-0.5,-1,-1.5],'r');
 %  clabel(C,h,'labelspacing',1000)
 
case 5, disp (' v_xx exact and numerical ') 
clf
 %  W1 = griddata(X,Y,VXXE,U1,V1,'cubic');
   W1 = griddata(X,Y,VXXE,U1,V1,'cubic');
   mesh(U1,V1,W1)
case 6, disp (' u_yy exact and numerical ') 
clf
  % W1 = griddata(X,Y,UYYE,U1,V1,'cubic');
   W1 = griddata(X,Y,UYY,U1,V1,'cubic');
   mesh(U1,V1,W1)
case 7, disp (' u exact and numerical ') 
   clf
   plot(-0.1,-0.1,'w.'), hold on
plot(1.1,1.1,'w.'), hold on
   W1 = griddata(X,Y,UE,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,5,'k'); hold on
   clabel(C,h,'labelspacing',1000)
   %[C,h] = contour(U1,V1,W1,[0,0.01,0.02],'k'); hold on
   W2 = griddata(X,Y,U,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W2,5,'r');
   clabel(C,h,'labelspacing',800)
   [C,h] = contour(U1,V1,W2,[0 0],'g');
case 8
   clf
   %W1 = griddata(X,Y,UXXE,U1,V1,'cubic');
   %[C,h] = contour(U1,V1,W1,[1.3,1,0.5,0,-0.5,-1,-1.3],'k'); hold on
   %clabel(C,h,'labelspacing',1000)
   %W1 = griddata(X,Y,UXXA,U1,V1,'cubic');
   %[C,h] = contour(U1,V1,W1,[1.3,1,0.5,0,-0.5,-1,-1.3],'r');
   %   [C,h] = contour(U1,V1,W1,10,'r');

   %clabel(C,h,'labelspacing',800)
   %mesh(U1,V1,W1)
   %axis tight
   UXXEC = pdeintrp(p,t,UXXE.');
   plot(UXXB - UXXEC) 
   
end
