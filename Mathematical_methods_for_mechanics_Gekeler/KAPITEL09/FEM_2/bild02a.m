function bild02a
% Geometrie von Spanner
clc, clear, format compact
FF1 = 'bsp02g';
SEGNR = [13,9,5,1,2,3,7,11,15,19,25,30,27,28,23,21,33,37,38,35,31,17];
SEG_LAST = [3,7];
SEG_LAGER = [28,37];
REFINE = 2;
[p,e,t,RAND,INNERPKTE] = prepar_n(FF1,REFINE,SEGNR);
% -- Ecken ------------
clf, hold on
plot(0,0,'w.'), hold on
plot(23,10,'w.'), hold on
axis equal tight, axis manual, grid on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
t1 = t(1:3,:);
hold on  % fuer flaches Bild --------
trimesh(t1',X,Y,Z1,'edgecolor','b'), hold on
axis equal, axis manual, grid on
for I = 1:size(e,2)
   A = [p(1,e(1,I));p(1,e(2,I))];
   B = [p(2,e(1,I));p(2,e(2,I))];
   plot(A,B,'k','linewidth',2), hold on
end
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
   plot(A,B,'bo'), hold on
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
   plot(A,B,'b*'), hold on
end
text(1.5,6.4,'(A)','fontsize',15)
text(1.5,5.4,'(A)','fontsize',15)
text(17,4.5,'(B)','fontsize',20)
GESAMTLAST = sqrt(60^2 + 210^2)
LAENGE  = norm(p(:,6) - p(:,2))
QUOTIENT = GESAMTLAST/LAENGE
clear
