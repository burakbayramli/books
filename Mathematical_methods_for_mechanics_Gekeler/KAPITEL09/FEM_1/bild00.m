function bild00(p,e,t)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
clf, hold on
DIST = 0.5;
X = p(1,:); Y = p(2,:); Z1 = zeros(1,length(X));
LU = [min(X),min(Y)]; LU = LU - DIST;
RO = [max(X),max(Y)]; RO = RO + DIST;
plot(LU(1),LU(2),'w.'), hold on
plot(RO(1),RO(2),'w.'), hold on
axis equal tight, axis  manual, grid on
if ~isempty(t)
   trimesh(t(1:3,:)',X,Y,Z1,'edgecolor','g'), hold on
end
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r','linewidth',2), hold on
      plot(A,B,'r.','markersize',6), hold on
   end
end
