function test01
% Diagrams for space craft APOLLO with SQP_H.m
% Comparison of results in US and SI units

clc, format long, format compact
load daten3a X Parmeter   % US units
Y = X; ParmeterY = Parmeter;
load daten3b X Parmeter   % SI units
nr = 100;
while ~ismember(nr,[1,2,3,4])
   nr = input(' Which diagram (1/2/3/4) ');
end   
n  = Parmeter(1); BTA = Parmeter(2); RX = Parmeter(4);
RRHO = Parmeter(5); T_END = Parmeter(6);
RY = ParmeterY(4);
TT = linspace(0,T_END,n+1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
U  = X(3*n+4:4*(n+1));   % Kontrolle
Y1 = Y(1:n+1); Y2 = Y(n+2:2*(n+1)); Y3 = Y(2*n+3:3*(n+1));
UY  = Y(3*n+4:4*(n+1));   % Kontrolle

%AA = 4/RY, BB = 1.22/RX 
%CC = 2.5/RY, DD = 0.76/RX
%XA = 4*RX/RY, XB = 2.5*RX/RY 

SDIV2M = 26600*0.3048^2/14.59
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
switch nr
case 1, disp(' Velocity, 10*1E5 km  ')
        disp(' Ex. 2 (SI): black, Ex. 1 (US): red ')
   clf
   plot(TT,X1,'k','linewidth',2), hold on %SI-units
   Y1 = 0.3048*Y1; %[ft] in [m]
   plot(TT,Y1,'r','linewidth',2)
case 2, disp(' Flight angle (Grad) ')
        disp(' Ex. 2 (SI): black, Ex. 1 (US): red ') 
   clf
   X2A = X2*180/pi;
   plot(TT,X2A,'k','linewidth',2), hold on
   Y2A = Y2*180/pi;
   plot(TT,Y2A,'r','linewidth',2)
case 3, disp(' Altitude (10*1E5 km) ')
        disp(' Ex. 2 (SI): black, Ex. 1 (US): red ') 
   clf
   X3 = RX*X3;
   plot(TT,X3,'k','linewidth',2), hold on
   Y3 = RY*0.3048*Y3; %[ft] in [m]
   plot(TT,Y3,'r','linewidth',2)
case 4, disp(' Control (rad)')
   clf
   plot(TT,U,'k','linewidth',2), hold on
   plot(TT,UY,'r','linewidth',2)
end

