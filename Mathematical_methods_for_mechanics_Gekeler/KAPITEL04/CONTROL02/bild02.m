function bild02
% Figure for Ex. 2, Orbit problem
load daten2 X T
clf
m = size(X,2); n = m-1;
DPHI = X(3,:)./X(1,:); WINKEL = zeros(1,m); AUX = 0;
for k = 1:m
   AUX       = AUX + T*DPHI(k)/n;
   WINKEL(k) = AUX;
end
T1 = linspace(0,pi,40);
plot(cos(T1),sin(T1),'g'), hold on
R1 = X(1,n+1);
plot(R1*cos(T1),R1*sin(T1),'b'), hold on

A  = X(1,:);          % sonst falsche Werte !!!
X1 = A.*cos(WINKEL); Y1 = A.*sin(WINKEL);
plot(X1,Y1,'r'), hold on
KONTROLLE  =  atan2(X(5,:),X(6,:));
U_ABS      =  WINKEL + 0.5*pi*ones(1,m) - KONTROLLE;
CONTROL    = [cos(U_ABS); sin(U_ABS)];
quiver(X1,Y1,CONTROL(1,:),CONTROL(2,:),0.5);
axis equal, grid on
title('Example 2','fontsize',18)
