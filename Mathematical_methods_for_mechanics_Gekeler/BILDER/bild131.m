% TRIANG.M ----------------------------------------------
% Zeichnet Dreieck
clf
X  = [-4; 4; 0; -4];
Y  = [0;  0; 4;  0];
XS = (X(1:3) + X(2:4))/2;
YS = (Y(1:3) + Y(2:4))/2;
plot(X,Y)
hold on
plot(X,Y,'.','MarkerSize',12);
hold on
plot(XS,YS,'o','MarkerSize',6);
hold on
r = 0.2;
N = 40;

ZX = X(1);
ZY = Y(1);
KX = zeros(N,1);
KY = zeros(N,1);
for I = 1:N
   KX(I) = ZX + r*cos(2*pi*(I-1)/(N-1));
   KY(I) = ZY + r*sin(2*pi*(I-1)/(N-1));
end
plot(KX,KY)
hold on

ZX = X(2);
ZY = Y(2);
KX = zeros(N,1);
KY = zeros(N,1);
for I = 1:N
   KX(I) = ZX + r*cos(2*pi*(I-1)/(N-1));
   KY(I) = ZY + r*sin(2*pi*(I-1)/(N-1));
end
plot(KX,KY)
hold on

ZX = X(3);
ZY = Y(3);
KX = zeros(N,1);
KY = zeros(N,1);
for I = 1:N
   KX(I) = ZX + r*cos(2*pi*(I-1)/(N-1));
   KY(I) = ZY + r*sin(2*pi*(I-1)/(N-1));
end
plot(KX,KY)
hold on
W = sqrt(2);
XN = [0;1;-1]/W;
YN = [-1;1/W;1/W];
N1 = [XS';YS'];
N2 = 2*[XN';YN'];
myquiver(N1,N2,0.2,0.1,'k',2,1);
hold on
text(-4,-0.5,'P_1');
text(4,-0.5,'P_2');
text(0,4.5,'P_3');

text(0.1,-0.4,'M_1');
text(2.3,2,'M_2');
text(-2.6,2,'M_3');

text(1,0.4,'S_1');
text(0.8,2.4,'S_2');
text(-2.6,1,'S_3');
axis([-5 5 -1 5]);
axis equal
%grid on
title('CR-Dreieck')
