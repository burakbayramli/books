function fig0409
% Figure 4.9

disp(' Call first DEMO1-5 ! ')
clf
load daten05 X Parmeter
n     = Parmeter(1);
XA    = X(1:n+1);         % Zustand
U     = X(n+2:2*(n+1));   % Kontrolle
X0    = Parmeter(2);
TSPAN = linspace(0,3,n+1);
plot(TSPAN,XA,'ko-','Linewidth',1,'markersize',6), hold on
plot(TSPAN(1:n),U(1:n),'ks-','Linewidth',1,'markersize',6)
hold on
%  -- exakte Loesung -------
T  = [0,1,2,3];
X1 = [1,0,0,1];
plot(T,X1,'k','Linewidth',2), hold on
T1 = [0,1];
U1 = [-1,-1];
plot(T1,U1,'k*-','Linewidth',2,'markersize',12), hold on
T2 = [1,2];
U2 = [0,0];
plot(T2,U2,'k*-','Linewidth',2,'markersize',12), hold on
T3 = [2,3];
U3 = [1,1];
plot(T3,U3,'k*-','Linewidth',2,'markersize',12), hold on
axis equal
grid on
%title('Beispiel 1','fontsize',22)
text(0.5,0.7,'x*','fontsize',22)
text(2.4,0.7,'x*','fontsize',22)
text(0.5,0.25,'x','fontsize',22)
text(2.4,0.25,'x','fontsize',22)
text(1.8,1.05,'u*','fontsize',22)
text(1.1,-0.95,'u*','fontsize',22)
text(1.8,0.5,'u','fontsize',22)
text(1.1,-0.5,'u','fontsize',2)
A           = 3/n;
FAKTOR      = ones(1,n+1);
FAKTOR(1)   = 0.5;
FAKTOR(n+1) = 0.5;
PERF_INDEX  = A*FAKTOR*XA + (XA(n+1) - 1)^2
disp('Exakter Wert = 1')
grid off
