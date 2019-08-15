function bild05
% Bilder zum Raumgleiter X-38 mit SQP_H.m
load daten13 X Parmeter
clf
Parmtr1 = Parmeter(1:9); 
n  = Parmtr1(1); BTA = Parmtr1(2); R = Parmtr1(4);
RRHO = Parmtr1(5); T_END = Parmtr1(6);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4:4*(n+1)); X5 = X(4*n+5:5*(n+1));
X6 = X(5*n+6:6*(n+1)); U = X(6*n+7:7*(n+1));
TT = linspace(0,T_END,n+1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,1)
X1A = X1*1E2;
plot(TT,X1A,'k','linewidth',2)
grid on
XMIN = min(X1A); XMAX = max(X1A);
axis([XMIN XMAX 0 T_END])
axis tight,
title('Velocity (km) ','fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,2)
YY = [TT;X2'];
YY = YY(:,1:n-1);
%plot(YY(1,:),YY(2,:),'r','linewidth',2), hold on
%plot(TT,X2,'r','linewidth',2), hold on

MOD2PI = 0;
if MOD2PI == 1
   Schranke = 2.5;
   X2A = X2;
   J = find(X2A < Schranke);
   X2A(J) = X2A(J) + 2*pi;
   X2A = X2A - 2*pi;
   plot(TT,X2A,'b','linewidth',2), hold on
end
% Mittelwerte fuer X2 bilden
X2M = [X2(1);(X2(2:n) + X2(1:n-1))/2;X2(n+1)];
YY = [TT;X2M'];
YY = YY(:,1:n-1);
plot(YY(1,:),YY(2,:),'b','linewidth',2), hold on
grid on
XMIN = min(YY(2,:)); XMAX = max(YY(2,:));
axis([XMIN XMAX 0 T_END])
axis tight,
title('Flight path angle (rad)','fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,3)
%X3 = X3 - R;
X3A = 100*R*X3;
plot(TT,X3A,'b','linewidth',2)
grid on
XMIN = min(X3A); XMAX = max(X3A);
axis([XMIN XMAX 0 T_END])
axis tight,
title('Altitude (km)','fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,4)
%plot(TT,U,'r','linewidth',2), hold on
%UA = U; J = find(UA < 3); UA(J) = UA(J) + 2*pi;
%UA = UA - 2*pi;
%plot(TT,UA,'b','linewidth',2)
% Mittelwerte fuer U bilden
U = [U(1);(U(2:n) + U(1:n-1))/2;U(n+1)];
%plot(TT,U,'b','linewidth',1)
U = [U(1);(U(2:n) + U(1:n-1))/2;U(n+1)];
YY = [TT;U'];
YY = YY(:,1:n-1);
plot(YY(1,:),YY(2,:),'k','linewidth',2), hold on
grid on
XMIN = min(YY(2,:)); XMAX = max(YY(2,:));
axis([XMIN XMAX 0 T_END])
axis tight,
title('Control (rad)','fontsize',12)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Hbild = 0;
if Hbild == 1
   pause
   clf
   HH = feval(@bsp16,X1,3,Parmeter);
   plot([1:length(HH)]',HH),hold on
   plot([1:length(HH)]',HH,'k.'),hold on
   title(' Residuum of h ')
   grid on
   pause
   clf
   GG = feval(@bsp16,X1,2,Parmeter);
   plot([1:length(GG)]',GG),hold on
   plot([1:length(GG)]',GG,'k.'),hold on
   title(' Residuum of g ')
   grid on
end


