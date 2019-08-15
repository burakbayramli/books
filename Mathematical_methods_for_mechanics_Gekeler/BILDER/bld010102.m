function bld010102
% BILD 002
% Skizze zum QR-Algorithmus
clc, clf
X = 0; Y = 0; U = 6; V = 0;
c = 0.07; d = 0.015;
myquiv(X,Y,U,V,c,d,'k',2), hold on   % x-Achse +
X = 0; Y = 0; U = -6; V = 0;
myquiv(X,Y,U,V,c,d,'k',2), hold on   % x-Achse -
X = 0; Y = 0; U = 5; V = 0;
myquiv(X,Y,U,V,c,d,'k',2), hold on   % ||V||E_1
X = 0; Y = 0; U = -5; V = 0;
myquiv(X,Y,U,V,c,d,'k',2), hold on   % -||V||E_1
X = 0; Y = 0; U = 0; V = 6;
myquiv(X,Y,U,V,c,d,'k',2), hold on   % y-Achse +
X = 0; Y = 0; U = 4; V = 3;
myquiv(X,Y,U,V,c,d,'k',2), hold on   % Vektor V
c1 = 0.12; d1 = 0.04;
X = 5; Y = 0; U = -1; V = 3;
myquiv(X,Y,U,V,c1,d1,'g',2), hold on   % Vektor W-
g = 0.05; h = 0.01;
X = -5; Y = 0; U = 9; V = 3;
myquiv(X,Y,U,V,g,h,'g',2), hold on  % Vektor  W+
X = [-3.4;5.6]; Y = X/3;
plot(X,Y,'--','linewidth',2), hold on              % Ebene H-
X = [-1.8;0.5]; Y = - 3*X;
plot(X,Y,'--','linewidth',2), hold on

X = linspace(-5,5,60);
Y = sqrt(25 - X.*X);
plot(X,Y,'linewidth',2), hold on
% -- Rahmen ---------------
RX = [-6,6,6,-6,-6];
RY = [-2,-2,6.5,6.5,-2];
plot(RX,RY,'k','linewidth',2)
%axis([-6 6 -1.5 7.5])
axis equal
text(4.1,-0.7,'|p|e_1','fontsize',26);
text(-5.6,-0.7,'- |p|e_1','fontsize',26);
text(2.6,1.7,'p','fontsize',26);
text(4,0.6,'u_-','fontsize',26);
text(-2,-1.3,'H_-','fontsize',26);
text(-1.9,1.8,'u_+','fontsize',26);
text(-2.3,3.4,'H_+','fontsize',26);
%title('Zum QR-Algorithmus','fontsize',18)
grid on
axis off
