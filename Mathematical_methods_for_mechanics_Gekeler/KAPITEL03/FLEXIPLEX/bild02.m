function bild02
% Beispiel Himmelblau, S. 153
clf,
plot(-1.5,-0.5,'w.'), hold on
plot(1.5,1.5,'w.'), hold on
axis equal tight, axis manual, grid on
%load daten PFAD
%plot(PFAD(:,1),PFAD(:,2),'r','linewidth',2), hold on
%plot(PFAD(:,1),PFAD(:,2),'r.','markersize',12), hold on
X = linspace(-1.5,1.5,60); Y = linspace(-0.5,1.5,60);
[U,V] = meshgrid(X,Y);
m = length(X); Z1 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      XX = [X(i),Y(k)];
      Z1(k,i) = feval('bsp01',XX);
   end
end
W1    = griddata(X,Y,Z1,U,V);
contour(U,V,W1,[75 75],'k'), hold on
contour(U,V,W1,[50 50],'k'), hold on
contour(U,V,W1,[25 25],'k'), hold on
contour(U,V,W1,[5 5],'k'), hold on
contour(U,V,W1,[1 1],'k'), hold on
contour(U,V,W1,[0.5 0.5],'k'), hold on
% LOESUNG:
circle(1,1,0.02,'k')
