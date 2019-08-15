function bild01a
% Eckart Gekeler, Universitaet Stuttgart, Release 3.9.09
% Plots CONTOUR and MESH for SOLUTION
load daten1 p e t EXAMPLE
load daten2 SOLUTION RD RC
Figure = 100;
%while ~ismember(Figure,[1,2])
%  Figure  = input(' Which figure?, (1/2) ');
%end  
Figure = 1;
clf, hold on
X = p(1,:); Y = p(2,:); Z = zeros(1,length(X));
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[U,V] = meshgrid(xlin,ylin);
W     = griddata(X,Y,Z,U,V,'cubic');
trimesh(t(1:3,:)',X,Y,Z,'edgecolor','g'), hold on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
flag = 0; % for test of boundary segments
if flag == 1
   I  = find(e(5,:) == 4); LI = length(I); % Boundary 2
   NN = e(1,I);
   for J = 1:length(NN)
      plot(X(NN(J)),Y(NN(J)),'ko','markersize',6), hold on
      pause
   end
end

LU = [min(X),min(Y)]; LU = LU - 0.2;
RO = [max(X),max(Y)]; RO = RO + 0.2;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(2),RO(2),'w.'), hold on
axis equal tight, axis manual, grid off
switch Figure
case 1, disp(' Contour for solution ')
   W = griddata(X,Y,SOLUTION,U,V,'linear'); %v4
   EXACT = (X - 5).^2 + Y.^2;
   W_EXACT = griddata(X,Y,EXACT,U,V,'linear'); %v4
   switch EXAMPLE
   case 1
      [C,h] = contour(U,V,W,[4,6,8,10,12],'k');
      clabel(C,h,'labelspacing',400);
      [C,h] = contour(U,V,W,[0,0],'k');
      clabel(C,h,'labelspacing',400);
      % weisseln(p,e,t) % Whitening of complement
   case 2
      [C,h] = contour(U,V,W,[10,13,16,19,22,25,28,31,34,37],'r'); hold on
      clabel(C,h,'labelspacing',400);
      [C,h] = contour(U,V,W_EXACT,[10,13,16,19,22,25,28,31,34,37],'k'); hold on
      % weisseln(p,e,t) % Whitening of complement
   end
case 2, disp(' Mesh ')
   clf
   W = griddata(X,Y,SOLUTION,U,V,'v4');
   W = W + 20;
   mesh(U,V,W), hold on
   view(3)
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r','linewidth',2), hold on
   end
end
clear
