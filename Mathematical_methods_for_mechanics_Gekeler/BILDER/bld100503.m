function bld100503
% Toruskoordinaten
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
U = linspace(-3,3,15);
TT = linspace(0,2*pi,160);
for I = 1:length(U)
   X = a*sinh(U(I))*ones(1,160)./(cosh(U(I)) - cos(TT));
   Z = a*sin(TT)./(cosh(U(I)) - cos(TT));
   plot(X,Z,'k','linewidth',1.5), hold on
end

V = linspace(0,2*pi,20);
TT = linspace(-3,3,160);
for I = 1:length(V)
   X = a*sinh(TT)./(cosh(TT) - cos(V(I)));
   Z = a*sin(V(I))*ones(1,160)./(cosh(TT) - cos(V(I)));
   plot(X,Z,'k','linewidth',1.5), hold on
end
X = [-5,5,5,-5,-5];
Y = [-5,-5,5, 5, -5];
plot(X,Y,'k','linewidth',2)
axis off
text(4.4,-0.5,'x','FontSize',24)
text(-0.6,4.6,'z','FontSize',24)
