function bild01
% Zeichnet CONTOUR und MESH fuer w, bzw. Triangulierung

load daten p t e w J w_exact
MAXERROR = max(abs(w - w_exact))

bilda = 100; KK = [1,2,3];
%while ~ismember(bilda,KK)
%   bilda = input('Which Figure ? (1/2/3) ');
%end
bilda = 1;
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(X,Y,w,U,V,'cubic');
trimesh(t(1:3,:)',X,Y,Z1), hold on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'k','Linewidth',1.5);
   hold on
end
switch bilda
case 1, disp(' Contour for w ')
   contour(U,V,W,10), hold on
  % [C,h] = contour(U,V,W,10);
  % clabel(C,h,'manual');
  for I = 1:length(J)
     XX = p(1,J(I)); YY = p(2,J(I));
     circle(XX,YY,0.03,'r'), hold on
  end
  axis equal 
case 2, disp(' Mesh for w ')
   clf
   mesh(U,V,W, 'linewidth',1,'edgecolor','k'), hold on
   hidden off
   X1 = p(1,e(1,:)); Y1 = p(2,e(1,:));
   X1 = [X1, X(1)];  Y1 = [Y1, Y(1)];
   w_e  = w(e(1,:)); w_e  = [w_e', w_e(1)];
   plot3(X1,Y1,w_e), hold on
   X2 = p(1,J); Y2 = p(2,J); Z2 = w(J);
   plot3(X2,Y2,Z2,'ro','MarkerSize',6), hold on
   Z3 = - ones(1,length(J));
   plot3(X2,Y2,Z3,'ro','MarkerSize',6), hold on
   plot3(X2,Y2,Z3,'r*','MarkerSize',6);
case 3, disp(' mesh for w - w_exact ')
   clf
   err = w - w_exact;
   W     = griddata(X,Y,err,U,V,'cubic');
   mesh(U,V,W,'linewidth',1,'edgecolor','k'), hold on
%   contour(U,V,W,10), hold on
  % [C,h] = contour(U,V,W,10);
  % clabel(C,h,'manual');
   X1 = p(1,e(1,:)); Y1 = p(2,e(1,:));
   X1 = [X1, X(1)];  Y1 = [Y1, Y(1)];
   err_e  = err(e(1,:)); err_e  = [err_e', err_e(1)];
   plot3(X1,Y1,err_e), hold on
end
%axis equal tight
%get(h)
clear
