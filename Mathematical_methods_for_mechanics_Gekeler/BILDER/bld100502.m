function bld100502
% Abgeflachte Drehellipsoidkoordinaten
clc, clf,, clear
a = 1;
c = 0.4; d = 0.15;
% -- X-Achse ------------
X = [-5,5];
Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-5,5];
arrow(X,Y,c,d,'k',2), hold on
axis equal tight, axis manual
U = linspace(0,3,15);
TT = linspace(0,2*pi,40);
for I = 1:length(U)
   X = a*cosh(U(I))*sin(TT);
   Z = a*sinh(U(I))*cos(TT);
   plot(X,Z,'k','linewidth',1.5), hold on
end
V = linspace(0,2*pi,15);
TT = linspace(0,3,20);
for I = 1:length(V)
   X = a*cosh(TT)*sin(V(I));
   Z = a*sinh(TT)*cos(V(I));
   plot(X,Z,'k','linewidth',1.5), hold on
end
X = [-5,5,5,-5,-5];
Y = [-5,-5,5, 5, -5];
plot(X,Y,'k','linewidth',2)
axis off
%grid on
text(4.4,-0.5,'x','FontSize',24)
text(-0.6,4.6,'z','FontSize',24)
