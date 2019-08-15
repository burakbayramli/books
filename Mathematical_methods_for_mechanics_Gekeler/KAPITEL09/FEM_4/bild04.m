function bild04
% Image for thermal flow in a cup
% Plots CONTOUR for Z,T,W, and quiver for V
clear
load daten4a p e t RAND IP Parmeter
load daten4b RDZ RCZ RDW RDT RCT
load daten4c V W Z T
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild?, (1/2/3/4) ');
end   
%bilda = 2;
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),30);
ylin    = linspace(min(Y),max(Y),30);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'v4');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','b'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual
LRCT = length(RCT);
EE = [RCT(1,:),RCT(2,LRCT)];
AA = p(1,EE); BB = p(2,EE);
plot(AA,BB,'k','linewidth',2),hold on
switch bilda
case 1, disp(' Contour for T ')
   W1    = griddata(X,Y,T,U1,V1,'cubic'); %'v4'  oder 'cubic'
%   [C,h] = contour(U1,V1,W1,10,'k'); hold on
   [C,h] = contour(U1,V1,W1,[55,50,45,40,35,30,25,20],'k'); hold on
   clabel(C,h,'labelspacing',500)
case 2, disp(' Contour for Z ')
   W1 = griddata(X,Y,Z,U1,V1,'cubic');
   [C,h] = contour(U1,V1,W1,10,'k'); hold on
   %clabel(C,h,'labelspacing',500)
case 3, disp(' Contour for W ')
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10,'k'); hold on
   %clabel(C,h,'labelspacing',500)
case 4, disp(' Quiver fuer V ')
   U1 = V(1,:); V1 = V(2,:);
%   XM = sum(p(1,t(1:3,:)),1)/3; FAILS SOMETIMES
%   YM = sum(p(2,t(1:3,:)),1)/3;
   M = size(t,2); XM = zeros(1,M); YM = XM;
   for I = 1:M
       XM(I) = sum(p(1,t(1:3,I)))/3;
       YM(I) = sum(p(2,t(1:3,I)))/3;
   end
   quiver(XM,YM,U1,V1)
end
%weisseln(p,e,t)
%clear
