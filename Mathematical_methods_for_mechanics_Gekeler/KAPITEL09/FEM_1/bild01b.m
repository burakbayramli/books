function bild01
% Eckart Gekeler, Universitaet Stuttgart, Release 13.4.05
% Zeichnet CONTOUR und MESH fuer LOESUNG
load daten1 p e t
load daten2 LOESUNG RD RC
Figure = 100;
%while ~ismember(Figure,[1,2])
%  Figure  = input(' Which figure?, (1/2) ');
%end  
Figure = 1;
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(X,Y,Z1,U,V,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end

%plot(X(46),Y(46),'ko','markersize',12), hold on  


%AUX = [e(1,:),e(1,1)];
%plot(p(1,AUX),p(2,AUX),'r','linewidth',2,'erasemode','none'), hold on
LU = [min(X),min(Y)]; LU = LU - 0.2;
RO = [max(X),max(Y)]; RO = RO + 0.2;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(2),RO(2),'w.'), hold on
axis equal tight, axis manual, grid off
switch Figure
case 1, disp(' Contour for solution ')
   W = griddata(X,Y,LOESUNG,U,V,'v4');
   pdemesh(p,e,t), hold on
   [C,h] = contour(U,V,W,[4,6,8,10,12]);
   clabel(C,h,'labelspacing',400);
  % weisseln(p,e,t) % Whitening of complement
case 2, disp(' Mesh ')
   clf
   W = griddata(X,Y,LOESUNG,U,V,'v4');
   W = W + 20;
   mesh(U,V,W), hold on
   view(3)
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r','linewidth',2), hold on
   end
end
%AUX = [e(1,:),e(1,1)];
%plot(p(1,AUX),p(2,AUX),'r','linewidth',2,'erasemode','none'), hold on
clear
