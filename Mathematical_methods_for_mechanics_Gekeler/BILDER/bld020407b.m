% BILD029B, Stabilitaetsbereiche der impliziten ADAMS-Verfahren
clf
clear
c = 0.3;
d = 0.1;
% -- X-Achse ---------
X = [-7,1];
Y = [0,0];
%arrow(X,Y,c,d,'k',2)
% -- Y-Achse -------
X = [0,0];
Y = [0,4];
arrow(X,Y,c,d,'k',2)
axis equal
PHI = linspace(0,2*pi,600);
Z = cos(PHI) + i*sin(PHI);
Z1 = Z;
Z2 = Z1.*Z;
Z3 = Z2.*Z;
Z4 = Z3.*Z;
Z5 = Z4.*Z;
Z6 = Z5.*Z;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% A-M p = 1
V = (Z - 1)./(0.5*Z + 0.5);
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
%plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-M, p = 2
V = (Z2 - Z)./((5/12)*Z2 + (2/3)*Z- (1/12));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-M, p = 3
V = (Z3 - Z2)./((9/24)*Z3 + (19/24)*Z2 - (5/24)*Z + (1/24));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-M, p=4
V = (Z4 - Z3)./((251/720)*Z4 + (323/360)*Z3 - (11/30)*Z2 + (53/360)*Z - (19/720));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-M, p = 5
V = (Z5 - Z4)./((95/288)*Z5 + (1427/1440)*Z4 - (133/240)*Z3 + (241/720)*Z2 ...
- (173/1440)*Z + (3/160));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
X = [-0.5,-0.6];
Y = [-0.1,0.9];
%arrow(X,Y,0.2,0.05,'k',1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%title('Implizite Adams-Verfahren','fontsize',22)
text(-1.2,3.6,'k = 1','fontsize',22)
text(-5.1,3.2,'k = 2','fontsize',22)
text(-3,1.8,'k = 3','fontsize',22)
text(-1.7,1.2,'k = 4','fontsize',22)
text(-1.5,0.5,'k = 5','fontsize',22)
axis equal tight
%grid on
