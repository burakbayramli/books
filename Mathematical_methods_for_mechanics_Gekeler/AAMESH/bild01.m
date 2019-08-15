function bild01(p,e,t,segnr1,segnr2,DIST)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
clf, hold on
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
LU = [min(X),min(Y)]; LU = LU - DIST;
RO = [max(X),max(Y)]; RO = RO + DIST;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on
axis equal tight, axis  manual, grid on
if ~isempty(t)
   trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
end
if ~isempty(segnr1)
   E = [];
   for I = 1:length(segnr1)
      J = find(e(5,:) == segnr1(I)); E  = [E ,e(1:2,J)];
   end
   for I = 1:size(E,2)
      A = [p(1,E(1,I));p(1,E(2,I))];
      B = [p(2,E(1,I));p(2,E(2,I))];
      plot(A,B,'r','linewidth',2), hold on
      plot(A,B,'r.','markersize',6), hold on
   end
end
if ~isempty(segnr2)
   E = [];
   for I = 1:length(segnr2)
      J = find(e(5,:) == segnr2(I)); E = [E,e(1:2,J)];
   end
   for I = 1:size(E,2)
      A = [p(1,E(1,I));p(1,E(2,I))];
      B = [p(2,E(1,I));p(2,E(2,I))];
      plot(A,B,'b','linewidth',2), hold on
      plot(A,B,'b.','markersize',6), hold on
   end
end
