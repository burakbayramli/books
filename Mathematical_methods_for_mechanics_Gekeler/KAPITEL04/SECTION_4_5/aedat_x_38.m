function aedat_x_38
clc, clf, format short e
% aerodynamische Daten der X-38:
%
% 1. Spalte: Geschwindigkeit in m/s
% 2. Spalte: Widerstandsbeiwert c_W
% 3. Spalte: Auftriebsbeiwert c_A

aerodat_x38 = ...
  [275.000e0,   0.30789e0,   0.21053e0;
   300.000e0,   0.27632e0,   0.23684e0;
   325.000e0,   0.26316e0,   0.24474e0;
   350.000e0,   0.24210e0,   0.25789e0;
   375.000e0,   0.23684e0,   0.26053e0;
   400.000e0,   0.28158e0,   0.23684e0;
   450.000e0,   0.31316e0,   0.23947e0;
   500.000e0,   0.34474e0,   0.26053e0;
   550.000e0,   0.37105e0,   0.28947e0;
   600.000e0,   0.38158e0,   0.29211e0;
   700.000e0,   0.39211e0,   0.31316e0;
   800.000e0,   0.39305e0,   0.30526e0;
   900.000e0,   0.39474e0,   0.31053e0;
  1000.000e0,   0.39500e0,   0.31579e0;
  1100.000e0,   0.39737e0,   0.31842e0;
  1200.000e0,   0.41579e0,   0.33684e0;
  1300.000e0,   0.42105e0,   0.34474e0;
  1400.000e0,   0.43421e0,   0.36842e0;
  1500.000e0,   0.44737e0,   0.38684e0;
  2000.000e0,   0.50000e0,   0.47105e0;
  2500.000e0,   0.52632e0,   0.52632e0;
  3000.000e0,   0.52895e0,   0.55263e0;
  3500.000e0,   0.53421e0,   0.55526e0;
  4000.000e0,   0.54737e0,   0.57368e0;
  4500.000e0,   0.55263e0,   0.58158e0;
  5000.000e0,   0.55790e0,   0.60526e0;
  5500.000e0,   0.55263e0,   0.61842e0;
  6000.000e0,   0.54737e0,   0.60000e0;
  6500.000e0,   0.53947e0,   0.60263e0;
  7000.000e0,   0.53421e0,   0.60526e0;
  7500.000e0,   0.52368e0,   0.61053e0];

XX = aerodat_x38;
XX(:,1) = XX(:,1);
N = size(XX,1);
A1 = XX(:,1);
A = [ones(N,1), A1, A1.*A1];
B = XX(:,2);
B = A'*B; C = A'*A;
U = C\B
B = XX(:,3);
B = A'*B;
V = C\B
% -- Ausgleichspolynome
TT = linspace(XX(1,1),XX(N,1),40);
YY1 = U(1) + U(2)*TT + U(3)*TT.*TT;
YY2 = V(1) + V(2)*TT + V(3)*TT.*TT;
plot(TT,YY1,'k:','linewidth',2), hold on   % CW
plot(TT,YY2,'r:','linewidth',2), hold on   % CA
plot(XX(:,1),XX(:,2),'k','linewidth',2), hold on
plot(XX(:,1),XX(:,3),'r','linewidth',2), hold on
text(6500,0.50,'c_W','fontsize',20)
text(6500,0.58,'c_A','fontsize',20)

%  Steigung  c_W
%pause
% Ableitungen
%clf
%CW_X = (XX(2:N,2) - XX(1:N-1,2))./(XX(2:N,1) - XX(1:N-1,1));
%CW_X = [CW_X;CW_X(N-1)];
%plot(XX(:,1),CW_X,'g','linewidth',2), hold on