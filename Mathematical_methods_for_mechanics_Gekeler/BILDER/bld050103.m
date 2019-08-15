function bld050103
% Cusp Katastrophe, Projektion
% z^3 + y*z + x = 0
clf
faktor = 1;
N = 40;
a = -1.5;
b = 0.5;
set(gcf,'renderer','zbuffer')
plot3(0,0,0.5,'.','markersize',6,'color','w'), hold on
plot3(0,0,-1,'.','markersize',6,'color','w'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X1 = [1,1];
Y1 = [a,b];
Z1 = [0,0];
plot3(X1,Y1,Z1,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X2 = [-1,1];
Y2 = [b,b];;
Z2 = [0,0];
plot3(X2,Y2,Z2,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X3 = [-1,-1];
Y3 = [a,b];
Z3 = [0,0];
plot3(X3,Y3,Z3,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X4 = [-1,1];
Y4 = [a,a];
Z4 = [0,0];
plot3(X4,Y4,Z4,'linewidth',2,'color','k'), hold on
%-- Falte 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y5 = linspace(a,0,N);
X5 = real(2*(-Y5/3).^(3/2));
Z5 = zeros(1,N);
plot3(X5,Y5,Z5,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-- Falte 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y6 = linspace(0,a,N);
X6 = - real(2*(-Y6/3).^(3/2));
Z6 = zeros(1,N);
plot3(X6,Y6,Z6,'linewidth',2,'color','k'), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawnow
X7 = [X5,X6];
Y7 = [Y5,Y6];
Z7 = [Z5,Z6];
fill3(X7,Y7,Z7,'y'),hold on
text(0.4,-0.5,0,'1 Loesung','fontsize',22)
text(-0.8,-0.5,0,'1 Loesung','fontsize',22)
text(-0.2,-1.4,0.2,'3 Loesungen','fontsize',22)

view(30,60)
xlabel('\lambda','fontsize',22)
ylabel('\mu','fontsize',22)
zlabel('x','fontsize',22)
grid on
