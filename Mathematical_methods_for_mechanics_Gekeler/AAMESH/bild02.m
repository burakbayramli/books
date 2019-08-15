function bild02
% Eckart Gekeler, Universitaet Stuttgart, Release 13.4.05
load daten p e t segnr1 segnr2 DIST
load daten5a KNOTEN
DIST = 0.5; t = [];
bild01(p,e,t,segnr1,segnr2,DIST)
N  = max(KNOTEN(3,:));
for I = 1:N
   K   = find(KNOTEN(3,:) ==  I);
   KN1 = KNOTEN(1:2,K);
   KN1 = [KN1,KN1(:,1)];
   plot(KN1(1,:),KN1(2,:),'g'), hold on
   plot(KN1(1,:),KN1(2,:),'.','Markersize',6), hold on
end
disp('Weiter mit bel. Taste')
pause
p = KNOTEN;
X = KNOTEN(1,:); Y = KNOTEN(2,:); Z = zeros(1,length(X));
t = delaunay(p(1,:),p(2,:)); t =  t';
flipud(t);
bild01(p,e,t,segnr1,segnr2,DIST)
disp('Weiter mit bel. Taste')
pause

%NE = size(e,2);
%e = [e,[e(2,NE);e(1,1);0;0;1]];
t = mesh27(p,e,t,segnr1,segnr2);
bild01(p,e,t,segnr1,segnr2,DIST)
