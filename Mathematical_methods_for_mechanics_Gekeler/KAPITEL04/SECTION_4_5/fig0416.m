function bild04a
% Diagrams to space craft X-38 with SQP_H.m
load daten12 X Parmeter
clf
Parmtr1 = Parmeter(1:9); 
n  = Parmtr1(1); BTA = Parmtr1(2); R = Parmtr1(4);
RRHO = Parmtr1(5); T_END = Parmtr1(6);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4:4*(n+1)); X5 = X(4*n+5:5*(n+1));
X6 = X(5*n+6:6*(n+1)); U = X(6*n+7:7*(n+1));
TT = linspace(0,T_END,n+1);
NN = 15;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,1)
X1A = X1*1E2;
plot(TT,X1A,'k','linewidth',2)
grid on
XMIN = min(X1A); XMAX = max(X1A);
axis([XMIN XMAX 0 T_END])
axis tight,
title('Velocity (km/s) ','fontsize',NN)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,2)
plot(TT,X2,'k','linewidth',2), hold on
grid on
XMIN = min(X2); XMAX = max(X2);
axis([XMIN XMAX 0 T_END])
axis tight,
title('Flight path angle (rad)','fontsize',NN)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,3)
X3A = 100*R*X3;
plot(TT,X3A,'k','linewidth',2)
grid on
XMIN = min(X3A); XMAX = max(X3A);
axis([XMIN XMAX 0 T_END])
axis tight,
title('Altitude (km)','fontsize',NN)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,4)
plot(TT,U,'k','linewidth',2), hold on
grid on
XMIN = min(U); XMAX = max(U);
axis([XMIN XMAX 0 T_END])
axis tight,
title('Control (rad)','fontsize',NN)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Hbild = 0;
if Hbild == 1
   pause
   clf
   HH = feval(@bsp15,X,3,Parmeter);
   plot([1:length(HH)]',HH),hold on
   plot([1:length(HH)]',HH,'k.'),hold on
   title(' Residuum of h ')
   plot([1:length(HH)]',HH),hold on
   grid on
end


