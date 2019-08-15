function bld050104
% Beispiel Crandall, Seydel79
% x - y*x*(x^2 - 1 + z^2) = 0
% 10*z + y*z(1 + 2*x^2 + z^2) = 0
clf
set(gcf,'renderer','zbuffer')
N = 40;
X0 = [0,0];
Y0 = [-0.5,13];
Z0 = [0,0];
plot3(X0,-Y0,Z0,'k','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y1 = linspace(1,12,N);
X1 = sqrt((Y1 - 1)./Y1);
Z1 = zeros(1,N);
plot3(X1,-Y1,Z1,'k','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y1 = linspace(1,12,N);
X1 = -sqrt((Y1 - 1)./Y1);
Z1 = zeros(1,N);
plot3(X1,-Y1,Z1,'k','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y2 = linspace(1,10,N);
X2 = zeros(1,N);
Z2 = sqrt((10 - Y2)./Y2);
plot3(X2,-Y2,Z2,'k-.','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y2 = linspace(1,10,N);
X2 = zeros(1,N);
Z2 = -sqrt((10 - Y2)./Y2);
plot3(X2,-Y2,Z2,'k-.','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y3 = linspace(4,5.5,N);
X3 = sqrt((11 - 2*Y3)./Y3);
Z3 = sqrt((3*Y3-12)./Y3);
plot3(X3,-Y3,Z3,'k--','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y3 = linspace(4,5.5,N);
X3 = -sqrt((11 - 2*Y3)./Y3);
Z3 = sqrt((3*Y3-12)./Y3);
plot3(X3,-Y3,Z3,'k--','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y3 = linspace(4,5.5,N);
X3 = sqrt((11 - 2*Y3)./Y3);
Z3 = -sqrt((3*Y3-12)./Y3);
plot3(X3,-Y3,Z3,'k--','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Y3 = linspace(4,5.5,N);
X3 = -sqrt((11 - 2*Y3)./Y3);
Z3 = -sqrt((3*Y3-12)./Y3);
plot3(X3,-Y3,Z3,'k--','linewidth',2), hold on
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plot3(0,-1,0,'k*','markersize',8), hold on
plot3(0,-5.5,3/sqrt(11),'k*','markersize',8), hold on
plot3(0,-5.5,-3/sqrt(11),'k*','markersize',8), hold on
plot3(0,-1,0,'k*','markersize',8), hold on
plot3(0,-10,0,'k*','markersize',8), hold on
plot3(sqrt(3)/2,-4,0,'k*','markersize',8), hold on
plot3(-sqrt(3)/2,-4,0,'k*','markersize',8), hold on
axis tight
%view(30,60)
xlabel('x_1','fontsize',22)
ylabel('-\mu','fontsize',22)
zlabel('x_2','fontsize',22)
grid on
