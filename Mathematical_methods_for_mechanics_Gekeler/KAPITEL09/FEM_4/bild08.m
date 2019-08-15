function bild08
% Image for convection in a box, contour plot
% with notations
clc, clear
load daten8a p e t RAND Parmeter
load daten8b RDZ RDW RDT RCT NACHBAR NORMALEN
load daten8c V W Z T
bilda = 100;
while ~ismember(bilda,[1,2,3])
   bilda = input(' Welches Bild?, (1/2/3) ');
end   
%bilda = 1;
clf, hold on
% -- Trimesh und Rand --------------------
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin    = linspace(min(X),max(X),20);
ylin    = linspace(min(Y),max(Y),20);
[U1,V1] = meshgrid(xlin,ylin);
W1      = griddata(X,Y,Z1,U1,V1,'cubic');
trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
SEGNR_A  = [37,34,31,28,25,22,19,16,13,10,7,3];
RDA = [];
for I = 1:length(SEGNR_A)
K  = find(e(5,:) == SEGNR_A(I)); LK = length(K); % Randsegment K
   RDA = [RDA,e(1,K)];
end
plot(p(1,RDA),p(2,RDA),'b','linewidth',2), hold on
axis equal, axis manual
% ----------------------------------
switch bilda
case 1, disp(' Contour for T ')
   W1    = griddata(X,Y,T,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   %[C,h] = contour(U1,V1,W1,[0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.9,0.9]); hold on
   % clabel(C,h,'labelspacing',1000)
case 2, disp(' Contour for Z ')
   ZA = 100*Z; %%%%%%%%%%%%%%%%%%%%%%
   W1 = griddata(X,Y,ZA,U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
   %[C,h] = contour(U1,V1,W1,[0.5,1,2,3,3.5,4,4.15],'k'); hold on
   %clabel(C,h,'labelspacing',1000)
case 3, disp(' Contour for W ')
   WA = 50*W;
   W1    = griddata(X,Y,WA',U1,V1,'v4');
   [C,h] = contour(U1,V1,W1,10); hold on
  % [C,h] = contour(U1,V1,W1,[1,2,3,4,5,6],'k'); hold on
  % clabel(C,h,'labelspacing',1000)
end
%clear
