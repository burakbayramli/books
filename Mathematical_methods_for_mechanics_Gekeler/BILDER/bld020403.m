% BILD025, Loesung DGL in dgl.m mit Trapezregel

clf
X = [1;0;-1];
A = [-21 19 -20;  19 -21 20; 40 -40 -40];
H = 0.1;
N =  2/H;
Z =  X;
B = eye(3,3) - A*H/2;
C = eye(3,3) + A*H/2;
for I = 1:N-1
 R = C*X;
 Y =  B\R;
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
%title('Bsp. 1: Trapezregel, h = 0.1')
text(0.03,0.9,'x(t)','Fontsize',24);
text(0.15,0.5,'y(t)','Fontsize',24);
text(0.15,-0.4,'z(t)','Fontsize',24);
