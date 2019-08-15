function bild01
% Example Himmelblau, p. 155
clf
plot(0,0,'w.'), hold on
plot(10,12,'w.'), hold on
axis equal tight, axis manual, grid on
%load daten PFAD
%plot(PFAD(:,1),PFAD(:,2),'r','linewidth',2), hold on
%plot(PFAD(:,1),PFAD(:,2),'r.','markersize',12), hold on
LOESUNG = [5, 6];
X = linspace(0,10,60); Y = linspace(0,12,60);
[U,V] = meshgrid(X,Y);
m = length(X); Z1 = zeros(m,m);
% Zielfunktion ------------------------
for i = 1:m
   for k = 1:m
      XX = [X(i),Y(k)];
      Z1(k,i) = feval('bsp00',XX);
   end
end
W1    = griddata(X,Y,Z1,U,V);
contour(U,V,W1,[2 2],'k'), hold on
contour(U,V,W1,[5 5],'k'), hold on
contour(U,V,W1,[10 10],'k'), hold on
contour(U,V,W1,[20 20],'k'), hold on
contour(U,V,W1,[30 30],'k'), hold on
circle(LOESUNG(1),LOESUNG(2),0.1,'k')
