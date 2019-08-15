function test06
% Check of arrow in space
clc,clf
az = 0; el = 0;  % gut
az = 120; el = 25;  % gut

set(gcf,'renderer','painter');
X = [0,2];
Y = [0,-2];
Z = [0,-2];
h = 0.5;
r = 0.05;
axis([-3 3 -3 3 -3 3])
plot3(X,Y,Z,'k','linewidth',1), hold on
view([az,el])
xlabel('x-Achse')
ylabel('y-Achse')
zlabel('z-Achse')
