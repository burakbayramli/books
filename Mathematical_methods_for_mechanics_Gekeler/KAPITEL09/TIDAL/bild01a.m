function bild01a
% Image for Island in a Bay
% Plots CONTOUR for H, Z and quiver for V
%clc
load daten1a p e t
load daten1b V SHALLOW
Z = V(3,:); V = V(1:2,:);
bilda = input(' Welches Bild?, (1/2/3/4/5) ');
%bilda = 2;
clf
I = find(e(5,:) == 2); LI = length(I);
% Boundary for FILL.M
RAND1 = [p(1,e(1,I));p(2,e(1,I))];
RAND2 = [0          ,  0 ,  10E3, 10E3, 0,   0;
         p(2,e(2,I(LI))), 16E3, 16E3, 0,    0,  p(2,e(1,I(1)))];
RAND = [RAND1,RAND2];

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
H = p(3,:);
hold on  % fuer flaches Bild --------
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
axis equal, axis manual
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
if ~isempty(SHALLOW)
   XX = p(1,SHALLOW); YY = p(2,SHALLOW);
  % plot(XX,YY,'ko'); hold on
end
switch bilda
case 1, disp(' Contour for H ')
   WW = H;
   W1 = griddata(X,Y,WW,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[8,5,4,2,1,0.5]); hold on
   clabel(C,h,'color','k','labelspacing',1000)
   insel(p,e), hold on
   fill(RAND(1,:),RAND(2,:),'y','erasemode','none'), hold on
case 2, disp(' Contour for Z ')
   WW = Z;
   W1 = griddata(X,Y,WW,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   %clabel(C,h,'color','k','labelspacing',1000)
   insel(p,e), hold on
   fill(RAND(1,:),RAND(2,:),'y','erasemode','none'), hold on
case 3, disp(' Flow ')
   plot(X(1),Y(1),'.','markersize',6);, hold on
   %myquiv(X,Y,V(1,:),V(2,:),'r'), hold on
   quiver(X,Y,V(1,:),V(2,:),'r'), hold on
   plot(X,Y,'.','MarkerSize',4), hold on
   GG = sqrt(V(1,:).*V(1,:) + V(2,:).*V(2,:));
   insel(p,e), hold on
   %fill(RAND(1,:),RAND(2,:),'y','erasemode','none'), hold on
   MAXVELOCITY = max(GG)
case 4, disp(' Surf for -H mal 10.E3 ')
   clf
   xlin    = linspace(min(X),max(X),40);
   ylin    = linspace(min(Y),max(Y),40);
   [U1,V1] = meshgrid(xlin,ylin);

   HH = -H*1000;
   W1 = griddata(X,Y,HH,U1,V1,'v4');
   surf(U1,V1,W1), hold on
   axis equal
   I = find(e(5,:) == 1); LI = length(I);
   A = e(1,I);
   I = find(e(5,:) == 2); LI = length(I);
   A = [A,e(1,I),e(2,I(LI))];
   X  = p(1,A); Y = p(2,A); Z = -p(3,A);
   plot3(X,Y,Z,'r','linewidth',2,'erasemode','none'), hold on
   I = find(e(5,:) == 3); LI = length(I);
   B = [e(1,I),e(2,I(LI))];
   X  = p(1,B); Y = p(2,B); Z = -p(3,B);
   plot3(X,Y,Z,'r','linewidth',2,'erasemode','none'), hold on
   fill3(X,Y,Z,'g','erasemode','none'), hold on
   view(30,70);
case 5, disp(' Surf for Z ')
   clf
   p = p/1000;
  % Z = - Z;
   X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
   xlin    = linspace(min(X),max(X),40);
   ylin    = linspace(min(Y),max(Y),40);
   [U1,V1] = meshgrid(xlin,ylin);
   W1 = griddata(X,Y,Z,U1,V1,'cubic');
   surf(U1,V1,W1), hold on
   axis([0 10 0 20 min(Z) max(Z)]);
  % axis equal, axis manual, grid on
   I = find(e(5,:) == 1); LI = length(I);
   A = e(1,I);
   I = find(e(5,:) == 2); LI = length(I);
   A = [A,e(1,I),e(2,I(LI))];
   X  = p(1,A); Y = p(2,A); Z = p(3,A);
  % plot3(X,Y,Z,'k','linewidth',2,'erasemode','none'), hold on
   I = find(e(5,:) == 3); LI = length(I);
   B = [e(1,I),e(2,I(LI))];
   X  = p(1,B); Y = p(2,B); Z = p(3,B);
  % plot3(X,Y,Z,'r','linewidth',2,'erasemode','none'), hold on
  % fill3(X,Y,Z,'g','erasemode','none'), hold on
   view(30,70);
end
clear
