function bild02
% Bild zum Spanner
clc, clear, format compact
load daten2a p e t RAND SEG_LAST SEG_LAGER
load daten2b RDU RDV LASTENU LASTENV
load daten2c LOESUNG SIGDIFF SIG1 SIG2 PHI EV1 EV2
bilda = 100; KK = [1,2,3,4,5];
while ~ismember(bilda,KK)
bilda = input(' Welches Bild? (1/2/3/4/5) ');
end
%bilda = 2;

% -- Ecken ------------
clf, hold on
plot(-0.5,-0.5,'w.'), hold on
plot(23.5,10.5,'w.'), hold on
axis equal tight, axis manual, grid off
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
xlin  = linspace(min(X),max(X),20);
ylin  = linspace(min(Y),max(Y),20);
[U,V] = meshgrid(xlin,ylin);
MIN_MAX_SIG1 = [min(SIG1),max(SIG1)]
MIN_MAX_SIG2 = [min(SIG2),max(SIG2)]

switch bilda

case 1, disp(' Geometrie ')
   t1 = t(1:3,:);
   %trimesh(t1',X,Y,Z1,'edgecolor','b'), hold on
   %for I = 1:size(e,2)
   %   A = [p(1,e(1,I));p(1,e(2,I))];
   %   B = [p(2,e(1,I));p(2,e(2,I))];
   %   plot(A,B,'k','linewidth',2), hold on
   %end
   for I = 1:size(RAND,2)
      A = [p(1,RAND(1,I));p(1,RAND(2,I))];
      B = [p(2,RAND(1,I));p(2,RAND(2,I))];
      plot(A,B,'r','linewidth',2), hold on
   end
   LAST = [];
   for I = 1:length(SEG_LAST)
      J  = find(e(5,:) == SEG_LAST(I)); EE = e(:,J);
      [U,K] = sort(EE(3,:)); EE = EE(:,K);
      LAST = [LAST,EE];
   end
   for I = 1:size(LAST,2)
      A = [p(1,LAST(1,I));p(1,LAST(2,I))];
      B = [p(2,LAST(1,I));p(2,LAST(2,I))];
      plot(A,B,'b','linewidth',2), hold on
   end
   LAGER = [];
   for I = 1:length(SEG_LAGER)
      J  = find(e(5,:) == SEG_LAGER(I)); EE = e(:,J);
      [U,K] = sort(EE(3,:)); EE = EE(:,K);
      LAGER = [LAGER,EE];
   end
   for I = 1:size(LAGER,2)
      A = [p(1,LAGER(1,I));p(1,LAGER(2,I))];
      B = [p(2,LAGER(1,I));p(2,LAGER(2,I))];
      plot(A,B,'g','linewidth',2), hold on
   end
case 2, disp(' Contour for SIGDIFF ')
   W     = griddata(X,Y,SIGDIFF,U,V,'cubic');
   [C,H] = contour(U,V,W,10);
case 3, disp(' Contour for SIG1 ')
   W     = griddata(X,Y,SIG1,U,V,'cubic');
   [C,H] = contour(U,V,W,10,'k');
case 4, disp(' Contour for SIG2 ')
   W     = griddata(X,Y,SIG2,U,V,'cubic');
   [C,H] = contour(U,V,W,10,'r');
case 5, disp(' Hauptspannungslinien ')
   EV11  = griddata(X,Y,EV1(1,:),U,V,'cubic');
   EV12  = griddata(X,Y,EV1(2,:),U,V,'cubic');
   streamslice(U,V,EV11,EV12,'noarrows','cubic'), hold on
   EV21  = griddata(X,Y,EV2(1,:),U,V,'cubic');
   EV22  = griddata(X,Y,EV2(2,:),U,V,'cubic');
   streamslice(U,V,EV21,EV22,'noarrows','cubic'), hold on
end
flag = 1;
if flag == 1
weisseln(p,RAND,t), hold on
end
for I = 1:size(RAND,2)
   A = [p(1,RAND(1,I));p(1,RAND(2,I))];
   B = [p(2,RAND(1,I));p(2,RAND(2,I))];
   plot(A,B,'r','linewidth',2), hold on
end
% -- RAHMEN -----------------
XXR = [0,23,23,0,0]; YYR = [0,0,10,10,0];
plot(XXR,YYR,'k','linewidth',2,'erasemode','none')
%axis off
clear
