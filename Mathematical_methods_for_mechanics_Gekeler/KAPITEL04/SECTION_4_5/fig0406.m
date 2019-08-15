function fig0406
% Figure 4.6, 

disp(' Call first DEMO1-2 ! ')
clf
load daten02 X Parmeter

n  = Parmeter(1); a = Parmeter(2); T_END = Parmeter(3);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4:4*(n+1));   % Kontrolle
% -- die beiden Kreise -----------
T1 = linspace(0,pi,40);
plot(cos(T1),sin(T1),'k','linewidth',2), hold on
R1 = X1(n+1);
plot(R1*cos(T1),R1*sin(T1),'k','linewidth',2), hold on
DPHI   = X3./X1; WINKEL = zeros(n+1,1); AUX    = 0;
for k = 1:n+1
   AUX       = AUX + T_END*DPHI(k)/n;
   WINKEL(k) = AUX;
end
X3 = X1.*cos(WINKEL); Y3 = X1.*sin(WINKEL);
plot(X3,Y3,'r','linewidth',2), hold on
U_ABS =  WINKEL + 0.5*pi*ones(n+1,1) - X4;
VEC   = [cos(U_ABS), sin(U_ABS)];
quiver(X3,Y3,VEC(:,1),VEC(:,2),0.7);
axis equal, grid off
