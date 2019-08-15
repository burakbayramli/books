% BILD029A, Stabilitaetsbereiche der expliziten ADAMS-Verfahren
clf
clear
c = 0.1;
d = 0.05;
% -- X-Achse -----------------
X = [-2.5,1.5];
Y = [0,0];
%arrow(X,Y,c,d,'k',2)
% -- Y-Achse ------------------
X = [0,0];
Y = [0,2];
arrow(X,Y,c,d,'k',2)
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
% A-B p = 1
V = Z -1;
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-B, p = 2
V = (Z2 - Z)./(1.5*Z - 0.5);
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-B, p = 3
V = (Z3 - Z2)./((23/12)*Z2 - (4/3)*Z + (5/12));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-B, p=4
V = (Z4 - Z3)./((55/24)*Z3 - (59/24)*Z2 + (37/24)*Z - (3/8));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-B, p = 5
V = (Z5 - Z4)./((1901/720)*Z4 - (1387/360)*Z3 + (109/30)*Z2 -(637/360)*Z ...
+ (251/720));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
plot(X,Y,'k','LineWidth',2);
hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A-B, p = 6
V = (Z6 - Z5)./((4277/1440)*Z5 - (7923/1440)*Z4 + ...
(9982/1440)*Z3 -(7298/1440)*Z2 + (2877/1440)*Z - (475/1440));
REV = real(V);
IMV = imag(V);
J = find(IMV >= 0);
X = REV(J);
Y = IMV(J);
J = find(Y < 2);
X = X(J);
Y = Y(J);
J = find(X < 1.5);
X = X(J);
Y = Y(J);
%plot(X,Y,'k','LineWidth',2);
%plot(X,Y,'.','Markersize',5);

hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
X = [-0.42,-0.42];
Y = [1.2,0.45];
arrow(X,Y,c,d,'k',1)
%title('Explizite Adams-Verfahren','fontsize',18)
text(-1.8,1.1,'k = 1','fontsize',22)
text(-1.52,0.3,'k = 2','fontsize',22)
text(-0.7,1.3,'k = 3','fontsize',22)
text(0.45,0.7,'k = 4','fontsize',22)
text(0.3,1.25,'k = 5','fontsize',22)
axis equal tight
%grid on
