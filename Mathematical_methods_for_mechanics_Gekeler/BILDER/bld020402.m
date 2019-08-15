% BILD024, Loesung DGL in dgl.m mit Euler Explizit

clf
X = [1;0;-1];
A = [-21 19 -20;  19 -21 20; 40 -40 -40];
H = 0.1;
N =  1/H;
Z =  X;
for I = 1:N-1
 Y =  X + H*A*X;
 X = Y;
 Z = [Z  X];
end
L = linspace(0,1,N);
plot(L,Z(1,:),'r','LineWidth',1.5)
hold on
plot(L,Z(2,:),'g','LineWidth',1.5)
hold on
plot(L,Z(3,:),'LineWidth',1.5)
hold on
grid on
%title(' Bsp. 1: EULER explizit, h = 0.05')
text(0.86,1E6,'x(t)','Fontsize',24);
text(0.86,-1E6,'y(t)','Fontsize',24);
text(0.73,-0.3E6,'z(t)','Fontsize',24);
text(0.05,1.05E6,'-- 10^6','fontsize',24)
