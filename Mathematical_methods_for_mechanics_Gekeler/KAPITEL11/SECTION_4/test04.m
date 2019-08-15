function demo4
% Abrollen von Scheibe A auf Scheibe B
% E.GEKELER, RELEASE 13.3.04
% Scheibe A : M1        : Mittelpunkt, X : Rand
% Scheibe B : M2 = (0,0): fester Mittelpunkt, Y : Rand
% anfaengl. Beruehrpunkt X(:,1) = Y(:,1)
% OPTION = 0: Scheibe A wandert um Scheibe B
% OPTION = 1: Scheibe A wandert auf und ab
% Zuerst Scheiben Konstruieren nach Vorlage
% Scheibe B positiv orientieren,
% Scheibe A positv orientieren, wenn A in B l"auft,
% sonst negativ orientieren
% ACHTUNG: Polygonzug NICHT schliessen, weil
% doppelte Punkte nicht vorkommen duerfen
% TOL : Toleranz fuer Abbruch in Bisektion
% NN : Anzahl Winkelschritte, gekoppelt mit TOL
clc, clf, format compact, format short g
K = [1,2,3,4]; done = 0;
flag = 0;
if flag == 1
   while ~done
      nr   = input(' Beispiel Nr. (1/2/3/4) ')
      done = ismember(nr,K);
   end;
end
% -- ALLGEMEINE PARAMETER ---------------------
OPTION = 0;
% ------------------------------------------
nr = 1;
switch nr
case 1
   % Zwei Zahnraeder, A auf B
   STEPNR  = 4;  % Anzahl Drehschritte
   TOL     = 1.0E-4; % Toleranz fuer Einfuegen
   A_AUF_B = 1;    % A auf/in B: 1/0
   GRAFIK = 1; NN = 60; % Winkelschritt = 2*pi/NN
   load scheibe26 XXA RRA % kleine Scheibe
   load scheibe27 XXB RRB % grosse Scheibes
   X = XXA; Y = XXB;
   % -- Verschiebung ----------------------
   M1 = RRA+RRB;
   %M1 = M1 - 0.05; X = X - 0.05;
% -- Bildjustierung -----------------------
clf, hold on
rr = 0.02;
plot(-RRB,-RRB,'w.'), hold on
plot(M1+RRA,RRB,'w.'), hold on

axis equal tight, axis manual, grid on

M2 = [0;0]; m = [M1;0];
XXA = X + m*ones(1,size(X,2));
XXB = Y;
plot(XXA(1,:),XXA(2,:),'r','linewidth',2), hold on
plot(XXB(1,:),XXB(2,:),'k','linewidth',2), hold on
%fill(XXB(1,:),XXB(2,:),'y')
M1 = [M1;0];
XXAL = size(XXA,2)
plot(XXA(1,347),XXA(2,347),'r*'), hold on
plot(XXB(1,31),XXB(2,31),'ko'), hold on

end
