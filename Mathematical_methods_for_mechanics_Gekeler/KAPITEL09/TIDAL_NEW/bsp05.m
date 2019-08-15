function [p,e,segnr1,segnr2,N1,N2] = bsp05
% Ring
% p sind die geordneten Knoten von Rand 1
% bzw. von Rand 1 und Rand 2

RADIUSA = 1; RADIUSB = 2;
N1 = 16; %Winkelunterteilung
J  = 1:N1;
X  = RADIUSB*cos(2*pi*(J-1)/N1);
Y  = RADIUSB*sin(2*pi*(J-1)/N1);
p1 = [X;Y];
AUX = (p1(:,2) + p1(:,N1))/2;
p1(:,1) = AUX;
%
N2 = 8;
J  = N2:-1:1;
X  = RADIUSA*cos(2*pi*(J-1)/N2);
Y  = RADIUSA*sin(2*pi*(J-1)/N2);
p2 = [X;Y];
p  = [p1,p2];

e1 = [N1, 1;
      1, 2];
e1 = [e1;0, 0.5; 0.5, 1; 
      ones(1,2);ones(1,2);zeros(1,2)];
         
e2 = [[2:N1-1];[3:N1]]; 
LE = size(e2,2); 
e2 = [e2;([1:LE]-1)/LE; [1:LE]/LE; 
      2*ones(1,LE);ones(1,LE);zeros(1,LE)];
      
e3 = [[1:N2-1];[2:N2]]; e3 = [e3,[N2;1]]; e3 = e3 + N1;
LE = size(e3,2);
e3 = [e3;([1:LE]-1)/LE; [1:LE]/LE;
      3*ones(1,LE);ones(1,LE);2*ones(1,LE)];
e  = [e1, e2, e3];
segnr1 = [1,2]; segnr2 = 3;
