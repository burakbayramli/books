function fig0417a(nr)
% Diagrams for space craft APOLLO and X-38 simplified
switch nr
case 1, load daten3a X Parmeter
case 2, load daten3b X Parmeter
case 3, load daten3c X Parmeter
case 4, load daten3d X Parmeter
 end
clf
n  = Parmeter(1); BTA = Parmeter(2); R = Parmeter(4);
RRHO = Parmeter(5); T_END = Parmeter(6);
TT = linspace(0,T_END,n+1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
if nr ~= 4
U  = X(3*n+4:4*(n+1));   % Kontrolle
else
U  = X(4*n+5:5*(n+1));   % Kontrolle
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,1)
plot(TT,X1,'k','linewidth',2)
grid on
switch nr
case 1, title('Velocity (10^5 ft)','fontsize',15)
case 2, title('Velocity (10^5 km)','fontsize',15)
case 3, title('Velocity (10^5 km)','fontsize',15)
case 4, title('Velocity (10^5 km)','fontsize',15)
end
XMIN = min(X1); XMAX = max(X1);
axis([XMIN XMAX, 0 T_END])
axis tight,
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,2)
%X2 = X2*180/pi;
plot(TT,X2,'k','linewidth',2)
grid on
XMIN = min(X2); XMAX = max(X2);
axis([XMIN XMAX, 0 T_END])
axis tight,
title('Flight angle (rad)','fontsize',15)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,3)
X3 = R*X3;
plot(TT,X3,'k','linewidth',2)
grid on
XMIN = min(X3); XMAX = max(X3);
axis([XMIN XMAX, 0 T_END])
axis tight,

switch nr
case 1, title('Height (10^5 ft)','fontsize',15)
case 2, title('Height (10^5 km)','fontsize',15)
case 3, title('Height (10^5 km)','fontsize',15)
case 4, title('Height (10^5 km)','fontsize',15)
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(2,2,4)
Controlfigure = 3;
switch Controlfigure
case 1
   U1 = U - 2*pi;
   %plot(TT,U,'r--','linewidth',2), hold on
   Schranke = 2.5;
   U2 = U;
   J = find(U2 < Schranke);
   U2(J) = U2(J) + 2*pi;
   U2 = U2 - 2*pi;
   plot(TT,U2,'b','linewidth',2), hold on
   grid on
   title('Control (rad)','fontsize',15)
case 2
   % Mittelwerte fuer U bilden
   U = [U(1);(U(2:n) + U(1:n-1))/2;U(n+1)];
   %plot(TT,U,'b','linewidth',1)
   U = [U(1);(U(2:n) + U(1:n-1))/2;U(n+1)];
   YY = [TT;U'];
   YY = YY(:,1:n-1);
   plot(YY(1,:),YY(2,:),'k','linewidth',2), hold on
   grid on
   title('Control (rad)','fontsize',15)
case 3   
   U = pi*U/180;
   plot(TT,U,'k','linewidth',2)
   grid on
   title('Control (rad)','fontsize',15)
   XMIN = min(U); XMAX = max(U);
   axis([XMIN XMAX, 0 T_END])
   axis tight,
end

