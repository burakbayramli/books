function bild07
% Image for shallow water in a Channel
clc
bilda = 100;
while ~ismember(bilda,[1,2])
   bilda = input(' Welches Bild? (1/2) ');
end   
%   Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN];
load daten7a p e t Parmeter 
load daten7b MONITOR_U MONITOR_Z

DT  = Parmeter(1); A = Parmeter(2); H = Parmeter(3);
L   = Parmeter(4); PERIOD = Parmeter(5); g = Parmeter(6);
TAU = Parmeter(8); NN = Parmeter(11);
omga   = 2*pi/PERIOD; %Frequenz
switch bilda 
case 1, disp(' Plot fuer unteren Rand des Kanals ')
   disp(' BLAU Wasserstand, ROT Geschwindigkeit ')
   clf, hold on
   MAXZ = max(max(abs(MONITOR_Z)));
   MAXU = max(max(abs(MONITOR_U)));
   MAXWERT = max(MAXZ,MAXU);
   I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
   J =  [e(1,I(1:LI)), e(2,I(LI))];
   XX = linspace(0,L,LI+1)';
   TT = linspace(0,NN*PERIOD,NN*PERIOD/DT+1);
   axis([0 L -MAXWERT MAXWERT])
   axis manual, grid on
   for K = 1:length(TT)
      T = TT(K);
      ITER_SEKUNDEN = [K,T]
      plot(XX,MONITOR_Z(K,J),'b','linewidth',2), hold on
      plot(XX,MONITOR_U(K,J),'r'),
      pause(0.1)
      set(gca,'nextplot','replacechildren');
   end
case 2, disp(' Plot fuer linkes und rechtes Kanalende ')
   clf, hold on
   I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
   J1 =  e(1,1); J2 = e(2,I(LI)); 
   MAXWERT = max(max(abs(MONITOR_Z(:,J1))),max(abs(MONITOR_Z(:,J2))))
   TT = linspace(0,NN*PERIOD,NN*PERIOD/DT+1)';
   axis([0 TT(end) -MAXWERT MAXWERT])
   axis manual, grid on
   plot(TT,MONITOR_Z(:,J1),'b'), hold on
   plot(TT,MONITOR_Z(:,J2),'r'), hold on
end






