function bild10
% Figure for exact example
% Plots CONTOUR for Z and W and quiver for V
load daten10b RSIDE NU DATA
load daten10a p e t KK MM RDZ RDW NACHBAR
load daten10c W Z tspan

bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild?, (1/2/3) ');
end
clf, hold on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
hold on  % fuer flaches Bild --------
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','y'), hold on
switch bilda
case 1, disp(' Contour for Z and Z exact ')
   WW = Z;
   W1 = griddata(X,Y,WW',U1,V1,'cubic');
   %[C,h] = contour(U1,V1,W1,5); hold on
   [C,h] = contour(U1,V1,W1,[-0.02,-0.05,-0.1,-0.15,-0.20,-0.25],'r','linewidth',2);
   hold on
   clabel(C,h,'labelspacing',1000)
   WW = DATA(1,:);
   W1 = griddata(X,Y,WW,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,[-0.02,-0.05,-0.1,-0.15,-0.20,-0.25],'k--','linewidth',2);
   clabel(C,h,'labelspacing',1000)
case 2, disp(' Contour for W and W exact ')
   WW = W';
   W1    = griddata(X,Y,WW,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'r'); hold on
   %[C,h] = contour(U1,V1,W1,5); hold on
   clabel(C,h,'labelspacing',1000)
   WW = DATA(2,:);
   W1    = griddata(X,Y,WW,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,[-6,-4,-2,0,2,4,6],'k--'); hold on
   clabel(C,h,'labelspacing',1000)
case 3, disp(' Contour fuer Fehler von Z ')
   WW = Z - DATA(1,:)';
   MAXZ = 100*max(abs(Z))
   WW = 100*WW; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   W1 = griddata(X,Y,WW',U1,V1,'cubic');
   % Exakter Fall -----------------
   [C,h] = contour(U1,V1,W1,[1,2,3,4,5,6,7,8],'k','linewidth',2); hold on
   % Naeherung ---------------------
   [C,h] = contour(U1,V1,W1,[0.1,0.2,0.3,0.4,0.5,0.6],'k','linewidth',2); hold on
   hold on
   clabel(C,h,'labelspacing',800,'fontsize',20)
   %clabel(C,h,'manual','fontsize',20)
end
%clear
