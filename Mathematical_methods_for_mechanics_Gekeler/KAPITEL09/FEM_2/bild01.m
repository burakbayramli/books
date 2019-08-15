function bild01
% Zeichnet QUIVER fuer LOESUNG und CONTOUR fuer SIGDIFF
% Bild 1/2 fuer kubische Elemente
% Bild 3/4 fuer SIGDIFF in den Schwerpunkten
clc,
bilda = input(' Welches Bild? (1/2/3/4/5/6) ');
%bilda = 1;
load daten1a p e t
load daten1b RDU RDV LASTENU LASTENV
load daten1c LOESUNG SIGDIFF SIG1 SIG2 PHI EV1 EV2
clf, hold on
plot(0,0,'w.'), hold on
plot(23,10,'w.'), hold on
axis equal tight, axis manual

X     = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U,V] = meshgrid(xlin,ylin);
tri   = t(1:3,:)';
trimesh(tri,X,Y,Z,'edgecolor','g'), hold on
for I = 1:length(e)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
switch bilda
case 1, disp(' Contour for SIGDIFF ')
   W     = griddata(X,Y,SIGDIFF,U,V,'cubic');
   [C,H] = contour(U,V,W,10);
   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
case 2, disp(' Contour for SIG1 ')
   W     = griddata(X,Y,SIG1,U,V,'cubic');
   [C,H] = contour(U,V,W,10,'k');
   %contour(U,V,W,[1E2,2E2,3E2,4E2,5E2,6E2,7E2,8E2,9E2],'k'), hold on
   %h     = findobj('Type','patch');
   %set(h,'Linewidth',2)
  % weisseln(p,e,t)
case 3, disp(' Contour for SIG2 ')
   W     = griddata(X,Y,SIG2,U,V,'cubic');
   [C,H] = contour(U,V,W,10,'r--');
  % contour(U,V,W,[1E2,2E2,3E2,4E2,5E2,6E2,7E2,8E2,9E2]'r--'), hold on
  % h     = findobj('Type','patch');
  % set(h,'Linewidth',2)
   %clabel(C,h,'labelspacing',400);
   %weisseln(p,e,t)

case 4, disp(' Support and Loads ')
   LAGERU = p(:,RDU(1,:));
   LAGERV = p(:,RDV(1,:));
   LASTENEU = [p(:,LASTENU(1,:)); LASTENU(2,:)];
   LASTENEV = [p(:,LASTENV(1,:)); LASTENV(2,:)];
   plot(p(1,RDU(1,:)),p(2,RDU(1,:)),'*','Markersize',6), hold on
   plot(p(1,RDV(1,:)),p(2,RDV(1,:)),'*','Markersize',6), hold on
   plot(p(1,LASTENU(1,:)),p(2,LASTENU(1,:)),'ko','Markersize',6)
   plot(p(1,LASTENV(1,:)),p(2,LASTENV(1,:)),'ko','Markersize',6)

case 5, disp(' Contour for SIG1 and SIG2 ')
   W     = griddata(X,Y,SIG1,U,V,'cubic');
   %[C,H] = contour(U,V,W,5); hold on
   h     = findobj('Type','patch');
   set(h,'edgecolor','g')
   [C,H] = contour(U,V,W,5); hold on
   drawnow
   pause(1)
   clear('edgecolor')
   W     = griddata(X,Y,SIG2,U,V,'cubic');
 %  [C,H] = contour(U,V,W,5); hold on
 %  h     = findobj('Type','patch');
 %  set(h,'edgecolor','b');
   %H = get(h)
case 6
   X     = p(1,:); Y = p(2,:);
   xlin  = linspace(min(X),max(X),20);
   ylin  = linspace(min(Y),max(Y),20);
   [U,V] = meshgrid(xlin,ylin);
   EV11  = griddata(X,Y,EV1(1,:),U,V,'cubic');
   EV12  = griddata(X,Y,EV1(2,:),U,V,'cubic');
   streamslice(U,V,EV11,EV12,'noarrows'), hold on
   EV21  = griddata(X,Y,EV2(1,:),U,V,'cubic');
   EV22  = griddata(X,Y,EV2(2,:),U,V,'cubic');
   streamslice(U,V,EV21,EV22,'noarrows'), hold on

   weisseln(p,e,t);
end
clear
