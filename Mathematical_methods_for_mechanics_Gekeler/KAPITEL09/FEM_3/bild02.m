function bild02
% Figure for half cylinder
% Plots CONTOUR for Z and W and quiver for V
%clc
load daten2a p e t RAND   
load daten2c W Z
V = velocity(p,e,t,Z);

bilda = 100;
%while ~ismember(bilda,[1,2,3])
%   bilda = input(' Which figure?, (1/2/3) ');
%end
bilda = 1;
clf, hold on
% -- Eckpunkte ------------------------
plot(-0.5,-0.5,'w.'), hold on
plot(12.5,4,'w.'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal tight, axis  manual, 
%text(9,4.3,'(A)','fontsize',18)  % Fuer Abb.
%text(16,0.7,'(C)','fontsize',18)
%text(0.4,2.5,'(B)','fontsize',18)
%text(18.2,2.5,'(D)','fontsize',18)

X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2,'erasemode','none'), hold on
end

switch bilda
case 1, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[4,3,2,1,0.5,0.1,0.05,0.01],'k','linewidth',2);
   %clabel(C,h,'labelspacing',1000)
   %clabel(C,h,'manual','fontsize',15)
   [C,h] = contour(U1,V1,W1,[0,-0.001,-0.005,-0.01,-0.05,-0.1,-0.3,-0.5,],'r');
   %clabel(C,h,'labelspacing',1000)
   %clabel(C,h,'manual','fontsize',10)
   %weisseln(p,RAND,t)
case 2, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'cubic');
 %  [C,h] = contour(U1,V1,W1,5); hold on
 %     clabel(C,h,'labelspacing',500)
   [C,h] = contour(U1,V1,W1,[-5,-3,-2,-1,-0.5,-0.3],'k'); hold on
   [C,h] = contour(U1,V1,W1,[1,2,3,4],'r'); hold on
case 3, disp(' Quiver fuer V ')
   U1 = V(1,:); V1 = V(2,:);
  % XM = sum(p(1,t(1:3,:)),1)/3;  FAILS
  % YM = sum(p(2,t(1:3,:)),1)/3;
   M = size(t,2); XM = zeros(1,M); YM = XM;
   for I = 1:M
       XM(I) = sum(p(1,t(1:3,I)))/3;
       YM(I) = sum(p(2,t(1:3,I)))/3;
   end
   quiver(XM,YM,U1,V1)
end
grid off
%axis off
%clear
