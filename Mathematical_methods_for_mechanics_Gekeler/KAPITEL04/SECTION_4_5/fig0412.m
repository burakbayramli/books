function fig0412
% Figure 4.12

disp(' Call first DEMO1-8 ! ')
clf
load daten08 X Parmeter
n           = Parmeter(1);
A           = 3/n;
FAKTOR      = ones(1,n+1);
FAKTOR(1)   = 0.5;
FAKTOR(n+1) = 0.5;
FAKTOR      = A*FAKTOR;

X1    = X(1:n+1);
U     = X(n+2:2*(n+1));
TSPAN = linspace(0,1,n+1)';
plot(TSPAN,X1,'k.-','Linewidth',2,'markersize',6), hold on
plot(TSPAN,U,'k*-','Linewidth',2,'markersize',6), hold on
%axis ([0 3 -3 3])
grid on
% -- RAND ---------------
X = [0, 1];
Y = [1.5, 1.5];
plot(X,Y,'k','Linewidth',2), hold on
X = [0.34, 1-0.34];
Y = [1.5, 1.5];
plot(X,Y,'.','color','k','Markersize',6), hold on
X_END =  U(n+1)

X8 = 0.345; Y8 = 1.5;
plot(X8,Y8,'ko','markersize',12), hold on
X8 = 1- X8;
plot(X8,Y8,'ko','markersize',12), hold on

%title('Beispiel 8','fontsize',22)
%text(1.1,0.6,'x^*','fontsize',22)
text(0.6,1.8,'boundary','fontsize',22)
text(0.8,1,'x','fontsize',22)
text(0.5,2.6,'u','fontsize',22)
grid off
