function test01
% Analytische Loesungen fuer Wellen in einem Kanal nach
% Lynch, D.R., Gray, W.G.: Analytical solutions for computer
% flow model testing. 
% J. Hydraul. Div. ASCE 104 (1978), 1409-1428. 
% Kanal rechts geschlossen; nur lineares Problem
% Konvection vernachlaessigt: z_x = 0

%Beispiel = 100;
%while ~ismember(Beispiel,[1,2])
%   Beispiel = input(' Welches Beispiel? (1/2) ');
%end   
Beispiel = 1;
switch Beispiel
case 1
   L =  160;  % Kanallaenge [m]
   H = 2;     % Wasserstand [m]
   T = 200;   % Periode am offenen Ende [sec]
   T = 20;
   A = 0.1;   % Amplitude [m]
   g = 9.81;  % Erdbeschleunigung [m/sec^2]
   tau = 0;   % Reibung am Kanalboden [1/sec]
   %tau = 0.02;
   omga = 2*pi/T; %Frequenz
   beta_2 = (omga^2 - sqrt(-1)*omga*tau)/(g*H);
   beta = sqrt(beta_2);
   Zeitraum = 200; % [sec.]
   TT = linspace(0,Zeitraum,Zeitraum+2);
   
   XX = linspace(0,160,40);
end
   clf, hold on
   axis([0 L -1 1])
   axis manual, grid on   
   plot([0,L],[0,0],'k'), hold on
   for K = 1:length(TT)
      T = TT(K);
      Z = real(A*exp(sqrt(-1)*omga*T)*cos(beta*(L - XX))/cos(beta*L));
      U = real(-sqrt(-1)*omga*A*exp(sqrt(-1)*omga*T)*sin(beta*(L - XX))/...
          (beta*H*cos(beta*L)));  
      P1 = plot(XX,Z,'b','linewidth',2); hold on
      P2 = plot(XX,U,'r','linewidth',2);
      pause(0.2)
      delete(P1); delete(P2);
   end

