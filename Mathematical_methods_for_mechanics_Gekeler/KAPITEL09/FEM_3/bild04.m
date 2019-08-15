function bild04
% Figure for back facing step
% Plots CONTOUR for Z and W and quiver for V
clc
load daten4a p e t RAND Parmeter
load daten4b RDZ RDW NACHBAR NORMALEN
load daten4c W Z
V = velocity(p,e,t,Z);

WMAX = max(W)
bilda = 100;
while ~ismember(bilda,[1,2,3])
  bilda = input(' Welches Bild?, (1/2/3) ');
end
%bilda = 1;
clf, hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis([0, 0.36, 0, 0.06]), axis equal, axis  manual
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
%trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
switch bilda
case 1, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,10); hold on
   flag = 0;
   if flag == 1
   [C,h] = contour(U1,V1,W1,[0.5 0.5]); hold on
   clabel(C,h,'color','r','labelspacing',1000)
   [C,h] = contour(U1,V1,W1,[0.4,0.3,0.2,0.1,0.03,0.02,0.01,0],'k'); hold on
   [C,h] = contour(U1,V1,W1,[-0.01, -0.02,0.03,-0.04]); hold on
   [C0,h0] = contour(U1,V1,W1,[-0.05, -0.05]); hold on
   clabel(C0,h0,'labelspacing',1000)
   end
case 2, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,5,'k'); hold on
   %clabel(C,h,'labelspacing',1000)
   [C,h] = contour(U1,V1,W1,[700,750],'r'); hold on
   %clabel(C,h,'labelspacing',1000)
case 3, disp(' Quiver fuer V ')
   U1 = V(1,:); V1 = V(2,:);
%   XM = sum(p(1,t(1:3,:)),1)/3;  FAILS
%   YM = sum(p(2,t(1:3,:)),1)/3;
   M = size(t,2); XM = zeros(1,M); YM = XM;
   for I = 1:M
       XM(I) = sum(p(1,t(1:3,I)))/3;
       YM(I) = sum(p(2,t(1:3,I)))/3;
   end
   quiver(XM,YM,U1,V1)
end
%clear
