function bld100501
% Paraboloid-Koordinaten
clc, clf,, clear
c = 0.8; d = 0.3;
% -- X-Achse ------------
X = [-10,10];
Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-10,10];
arrow(X,Y,c,d,'k',2), hold on
axis equal tight, axis manual
V = [0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7];
U = [0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7];

TT = linspace(-6,6,20);
for I = 1:length(V)
   X = V(I)*TT;
   Z = (TT.*TT - V(I)*V(I))/2;
   plot(X,Z,'k','linewidth',1.5), hold on
end

for I = 1:length(V)
   X = U(I)*TT;
   Z = (U(I)*U(I) - TT.*TT)/2;
   plot(X,Z,'k','linewidth',1.5), hold on
end
X = [-10,10,10,-10,-10];
Y = [-10,-10,10, 10, -10];
plot(X,Y,'k','linewidth',2)
axis off
text(8.8,-1,'x','FontSize',24)
text(-1,9,'z','FontSize',24)
%axis off
