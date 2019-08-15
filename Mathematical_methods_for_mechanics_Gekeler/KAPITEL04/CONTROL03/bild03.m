function bild03
% Figure for example 3, Dyer-McReynolds, p. 66

load daten3 t1 X U
clf
m = length(U); n = m-1;
DPHI    = X(3,:)./X(1,:);
WINKEL  = zeros(1,m);
AUX = 0;
for k = 1:m
   AUX       = AUX + DPHI(k);
   WINKEL(k) = AUX;
end
WINKEL  = t1*WINKEL/n;
T1 = linspace(0,WINKEL(m),40);
plot(cos(T1),sin(T1),'g'), hold on  % Anfangsorbit
R1 = X(1,n+1);
plot(R1*cos(T1),R1*sin(T1),'b'), hold on % Endorbit
A  = X(1,:);          % sonst falsche Werte!!!
X1 = A.*cos(WINKEL); Y1 = A.*sin(WINKEL);
plot(X1,Y1,'r'), hold on
U_ABS   =  WINKEL + 0.5*pi*ones(1,m) - U;
CONTROL = [cos(U_ABS); sin(U_ABS)];
quiver(X1,Y1,CONTROL(1,:),CONTROL(2,:),0.2);
%  axis([-1 6 -2 1])
axis equal, grid on
clear
