function test02

% Testen von Geometrie und Aussenrand
% Transport problem
[p,e,t] = bsp09;
REFINE = 1;
for J = 1:REFINE
   [p,e,t] = mesh01_t([],p,e,t);
end   
RAND = e; E = e;

clf, hold on
X = p(1,:); Y = p(2,:);Z1 = zeros(1,length(X));
xlin  = linspace(min(X),max(X),30);
ylin  = linspace(min(Y),max(Y),30);
[U1,V1] = meshgrid(xlin,ylin);
W1     = griddata(X,Y,Z1,U1,V1,'v4');
tri   = t(1:3,:)';
trimesh(tri,X,Y,Z1,'edgecolor','b','linewidth',1), hold on
%plot(X,Y,'.','markersize',6),hold on
axis equal, grid on
for I = 1:length(RAND)
    II = [RAND(1,I) ,RAND(2,I)]
    plot(X(RAND(1,I)),Y(RAND(1,I)),'ro','markersize',6),hold on
    pause
end
clear
