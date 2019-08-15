function demo1
% Drei Beispiele fuer Mehrkoerperprobleme
% Beispiel 1: Pendelgleichung als Mech. System
%             Hairer, II, VI.5
% Beispiel 2: Sieben-Koerper-Problem Schiehlen 1990.
%             Hairer, II, VI.9
% Beispiel 3: Doppelpendel als Mech. System
% ------------------------------------------------------
% Wenn w0 nicht konsistent ist, muss es im ersten Schritt
% des RKV berechnet werden
clear, clc, format compact
nr = 100; KK = [1,2,3];
while ~ismember(nr,KK)
   nr   = input(' Beispiel Nr. (1/2/3) ');
end
hem = 10; KK = [3,4];
while ~ismember(hem,KK)
   hem   = input(' Verfahren HEM3 oder HEM4 (3/4) ');
end;
% ------------------------------------------------------
switch nr
case 1, disp(' Pendelgleichung ')
   Eps     = 1e-4; %lokale Toleranz ev. Erhoehen !
   tau_max = 2;    % Max. Schrittweite
   tau     = 0.1;  % anfaengl. Schrittschaetzung
   Parmtr2 = [Eps,tau_max,tau,hem]; % Parameter fuer RKV
   g = 9.81; m = 1; L = 2;
   Parmtr3 = [g,m,L];           % Parameter fuer DGL
   % ------------------------------------------------
   u = [0;-2]; v = [1;0];  % konsistente Anfangswerte
   t0 = 0; t_end = 10;     % Anf.- und Endzeitpunkt
   % Berechnung von dv und w, dv wird hier nicht gebraucht
   w = 0; FF = 'bsp01';
   F  = feval(FF,1,t0,u,v,w,Parmtr3);
   H  = zeros(1,1);
   G1 = feval(FF,3,t0,u,v,w,Parmtr3);
   M  = feval(FF,5,t0,u,v,w,Parmtr3);
   G2 = feval(FF,3,t0,u,v,w,Parmtr3);
   AA = [M, G1';G2, 0];
   %CN = cond(A);
   RS  = [F;H]; XX = AA\RS;
   dv = XX(1:2); w  = XX(3);
   % -------------------------------------------------
   [tt,uout,vout,wout,errorcode] = ...
   hem4('bsp01',t0,t_end,u,v,w,Parmtr2,Parmtr3);
   %save daten1 tt uout vout wout
   UU = uout';
   clf
   plot(tt,uout(1,:),'k','linewidth',2), hold on
   plot(tt,uout(2,:),'r','linewidth',2), hold on
   ZZ = uout(1,:).*uout(1,:) + uout(2,:).*uout(2,:);
   ZZ = ZZ - L*L;
   plot(tt,ZZ,'g'), hold on
   grid on
   %axis equal
case 2, disp(' Andrews Squeezer ')
   % u1 = beta, u2 = Theta, u3 = gama, u4 = Phi,
   % u5 = delta, u6 = Omega, u7 = epsilon
   Eps       = 1e-5; %lokale Toleranz
   tau_max   = 0.0002;    % Max. Schrittweite
   tau       = 0.0002;  % anfaengl. Schrittschaetzung
   Parmeter2 = [Eps,tau_max,tau,hem]; % Parameter fuer RKV
   mom       = 0.033;               % Drehmoment
   Parmeter3 = mom;                % Parameter fuer DGL
   t0 = 0; t_end = 0.03;     % Integrationsintervall
   % -- konsistente Anfangswerte  -------------
   u    = zeros(7,1);
   u(1) = - 0.0617138900142764496358;
   u(2) =   0;
   u(3) =   0.4552798191630703802559;
   u(4) =   0.222668390165885884674;
   u(5) =   0.4873649795438425502255;
   u(6) = - 0.222668390165885884674;
   u(7) =   1.2305474445498211924973;
   v    =   zeros(7,1);
   w    =   zeros(6,1);
   w(1) =   98.56687039624108960576549;
   w(2) = - 6.122688344255662655031;
   [tt,uout,vout,wout,errorcode] = ...
       hem4('bsp02',t0,t_end,u,v,w,Parmeter2,Parmeter3);
   save daten2 tt uout vout wout
   bld060803
case 3, disp(' Doppelpendel ')
   Eps     = 1e-5; %lokale Toleranz
   Eps = 1e-3;
   tau_max = 2;    % Max. Schrittweite
   tau     = 0.1;  % anfaengl. Schrittschaetzung
   Parmtr2 = [Eps,tau_max,tau,hem]; % Parameter fuer RKV
   g = 9.81; m = 1; L = 2;
   Parmtr3 = [g,m,L];           % Parameter fuer DGL
   % ------------------------------------------------
   u = [0;-2;2;-2]; v = [1;0;0;-1];  % konsistente Anfangswerte
   t0 = 0; t_end = 10;     % Anf.- und Endzeitpunkt
   % Berechnung von dv und w, dv wird hier nicht gebraucht
   w = 0; FF = 'bsp03';
   F  = feval(FF,1,t0,u,v,w,Parmtr3);  H  = zeros(2,1);
   G1 = feval(FF,3,t0,u,v,w,Parmtr3);
   M  = feval(FF,5,t0,u,v,w,Parmtr3);
   G2 = feval(FF,3,t0,u,v,w,Parmtr3);
   AA = [M, G1';G2, zeros(2,2)];
   %CN = cond(A);
   RS  = [F;H]; XX = AA\RS;
   dv = XX(1:4); w  = XX(5:6);
   % -------------------------------------------------
   [tt,uout,vout,wout,errorcode] = ...
   hem4('bsp03',t0,t_end,u,v,w,Parmtr2,Parmtr3);
   save daten3 tt uout vout wout
   bld060801
end
disp(' ------------------- ')
if errorcode == 0
   disp(' Loesung ');
else
   disp(' keine oder schlechte Loesung ');
   if errorcode == 1, disp(' Max. Schrittzahl erreicht '), end
   if errorcode == 2, disp(' Min. Schrittweite erreicht '), end
end;

