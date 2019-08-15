function bld060507
% Plot of V in ABRAHAM, S. 215
clc, clf
m = 70;
MU = 0.012277471;
EE = [-MU;0]; MM = [1-MU;0];
% -- Rahmen --------------------------
%X0 = [0;0];
%XR = [-1.5,1.5,1.5,-1.5,-1.5]; YR = [-1.5,-1.5,1.5,1.5,-1.5];
%plot(2.6,2.6,'w.'), hold on
%plot(-2.6,-2.6,'w.'), hold on
%axis equal tight, axis  manual
X = [linspace(-2,2,m);linspace(-2,2,m)];
[U,V] = meshgrid(X(1,:),X(2,:));
Z = zeros(m,m); 
for I = 1:m
   for K = 1:m
   RHO = sqrt((X(1,I)-(1-MU))^2 + X(2,K)^2);
   SIGMA = sqrt((X(1,I) + MU)^2 + X(2,K)^2);
   AUX = -0.5*(X(1,I)^2 + X(2,K)^2) - MU/RHO - (1-MU)/SIGMA;
   if abs(AUX) <= 3.5; 
      Z(K,I) = AUX; else Z(K,I) = -3.5; end
   end
end
W1 = griddata(X(1,:),X(2,:),Z,U,V);
%contour(U,V,W1)
mesh(U,V,Z,W1), hold on
plot3(-1,0,-1.5,'k.','markersize',18), hold on
X = 0.5-MU; Y = sqrt(3)/2;
   RHO = sqrt((X-(1-MU))^2 + Y^2);
   SIGMA = sqrt((X + MU)^2 + Y^2);
   V = -0.5*(X^2 + Y^2) - MU/RHO - (1-MU)/SIGMA ;
plot3(X,Y,V,'k.','markersize',18,'erasemode','none'), hold on
plot3(X,-Y,V,'k.','markersize',18,'erasemode','none'), hold on
X = 0.83; Y = 0;
   RHO = sqrt((X-(1-MU))^2 + Y^2);
   SIGMA = sqrt((X + MU)^2 + Y^2);
   V = -0.5*(X^2 + Y^2) - MU/RHO - (1-MU)/SIGMA;
plot3(X,Y,V,'k.','markersize',18,'erasemode','none'), hold on
X = 1.15; Y = 0;
   RHO = sqrt((X-(1-MU))^2 + Y^2);
   SIGMA = sqrt((X + MU)^2 + Y^2);
   V = -0.5*(X^2 + Y^2) - MU/RHO - (1-MU)/SIGMA;
plot3(X,Y,V,'k.','markersize',18,'erasemode','none'), hold on


view(-37.5,30)
%view(70,30)
axis equal
grid on
xlabel('x-axis','fontsize',18)
ylabel('y-axis','fontsize',18)

%axis off
clear
