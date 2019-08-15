function bild05
% Figure for transport problem
% Plots CONTOUR for Z and W and quiver for V
%clc
load daten5a p e t RAND Parmeter
load daten5b RDZ RDW NACHBAR NORMALEN
load daten5c V W Z
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild? (1/2/3) ');
end
%bilda = 1;
clf, hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
axis equal, axis  manual, grid off
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
if bilda == 1 % Contour for Z
   W1 = griddata(X,Y,Z',U1,V1,'v4');
  % [C,h] = contour(U1,V1,W1,10); hold on
   [C,h] = contour(U1,V1,W1,[0.1,1,5,10,15,20,25],'k'); hold on
   clabel(C,h,'labelspacing',1000)
   [C,h] = contour(U1,V1,W1,[-0.2,-0.4,-0.6],'r'); hold on
   clabel(C,h,'labelspacing',1000)

end
if bilda == 2 % contour for W
   W1    = griddata(X,Y,W',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,5); hold on
   clabel(C,h,'labelspacing',1000)
end
if bilda == 3 % quiver for V
   U1 = V(1,:); V1 = V(2,:);
   M = size(t,2); XM = zeros(1,M); YM = XM;
   for I = 1:M
       XM(I) = sum(p(1,t(1:3,I)))/3;
       YM(I) = sum(p(2,t(1:3,I)))/3;
   end
   quiver(XM,YM,U1,V1)
end

end
%clear
