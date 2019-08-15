function fig0410
% Figure 4.10

disp(' Call first DEMO1-6 ! ')
load daten06 X Parmeter
clf
n       = Parmeter(1);
m       = 10; %fuer exakte Loesung
XA      = X(1:n+1);
U       = X(n+2:2*(n+1));
TSPAN   = linspace(0,3,n+1);
plot(TSPAN,XA,'ko-','Linewidth',2,'markersize',6), hold on
plot(TSPAN(1:n),U(1:n),'ks-','Linewidth',2,'markersize',6), hold on
% exakte Loesung
% Intervall (0,1) -----------
T1  = [0,1]; X1 = [0,0];
plot(T1,X1,'k','Linewidth',2), hold on
U1 = [0,0];
plot(T1,U1,'k*-','Linewidth',2,'markersize',6), hold on
% Intervall (1,2) -------------
T2 = linspace(1,2,m);
X2 = 1 - (T2 - 2).*(T2 - 2);
plot(T2,X2,'k','Linewidth',2), hold on
U2 = 2*(2 - T2);
plot(T2,U2,'k*-','Linewidth',2,'markersize',6), hold on
% Intervall (2,3) --------------
T3 = [2, 3]; X3 = [1, 1]; U3 = [0, 0];
plot(T3,X3,'k','Linewidth',2), hold on
plot(T3,U3,'k*-','Linewidth',2,'markersize',6), hold on
axis equal
grid on
%title('Beispiel 2','fontsize',22)
text(1.25,0.3,'x*','fontsize',22)
text(2.8,0.88,'x*','fontsize',22)
text(2.6,1.2,'x','fontsize',22)
text(1.3,1.6,'u*','fontsize',22)
text(0.7,-0.15,'u*','fontsize',22)
text(2.6,-0.15,'u*','fontsize',22)
text(0.7,0.6,'u','fontsize',22)
text(2.5,0.15,'u','fontsize',22)

A           = 3/n;
FAKTOR      = ones(1,n+1);
FAKTOR(1)   = 0.5;
FAKTOR(n+1) = 0.5;
grid off
