function bld020408
% Stabilitaetsbereiche Rueckwaertsdifferenzenverfahren
clf
clear
z = [ 0   0;
     -1  25];
plot(z(1,:),z(2,:),'LineWidth',2);
hold on
z = [-8  30; 0  0];
plot(z(1,:),z(2,:),'LineWidth',2);
hold on
%axis equal
PHI = linspace(0,2*pi,600);
Z = cos(PHI) + i*sin(PHI);
Z1 = Z;
Z2 = Z1.*Z;
Z3 = Z2.*Z;
Z4 = Z3.*Z;
Z5 = Z4.*Z;
Z6 = Z5.*Z;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% BDF p = 1
V = (Z - 1)./Z;
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BDF, p = 2
V = (3*Z2 - 4*Z + 1)./(2*Z2);
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BDF, p = 3
V = (11*Z3 - 18*Z2 + 9*Z1 - 2)./(6*Z3);
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BDF, p=4
V = (25*Z4 - 46*Z3 + 36*Z2 - 16*Z1 + 3)./(12*Z4);
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BDF, p = 5
V = (137*Z5 - 300*Z4 + 300*Z3 - 200*Z2 + 75*Z1 - 12)./(60*Z5);
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BDF, p = 6
V = (147*Z6 - 360*Z5 + 450*Z4 - 400*Z3 + 225*Z2 - 72*Z1 + 10)./(60*Z6);
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
A = [-5, 1.2];
B = [-2.8, 0.5];
arrow_4(A,B,0.8,0.4,'k',1)
A = [4.5, 2];
B = [-3,1.8];
arrow_4(A,B,0.8,0.4,'k',1)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%title(' Oberer Rand von S fuer BDF, p = 1-6','fontsize',18)
text(-8,-3.7,'p = 1','fontsize',22)
text(2,-3.7,'p = 2','fontsize',22)
text(5.5,4,'p = 3','fontsize',22)
text(8,7,'p = 4','fontsize',22)
text(13,11,'p = 5','fontsize',22)
text(18,19,'p = 6','fontsize',22)
%axis equal
grid off
