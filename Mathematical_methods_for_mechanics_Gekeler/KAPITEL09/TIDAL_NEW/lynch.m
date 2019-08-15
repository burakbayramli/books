function lynch
% Analytische Loesungen fuer Wellen in einem Kanal nach
% Lynch, D.R., Gray, W.G.: Analytical solutions for computer
% flow model testing. 
% J. Hydraul. Div. ASCE 104 (1978), 1409-1428. 
% Kanal rechts geschlossen; ??
% Konvection vernachlaessigt: z_x = 0
load daten5a p e t Parmeter FF2 Example HOURS
switch Example
   case 1, load daten5b MONITOR_U MONITOR_Z 
   case 2, load daten5c MONITOR_U MONITOR_Z 
   case 3, load daten5d MONITOR_U MONITOR_Z 
   case 4, load daten5e MONITOR_U MONITOR_V MONITOR_Z

end

%Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU_E];
DT = Parmeter(1);
L = Parmeter(4); H = Parmeter(3); T = Parmeter(5);
A = Parmeter(2); g = 10;  
   tau = 0;   % Reibung am Kanalboden [1/sec]
omga = 2*pi/T; %Frequenz
beta_2 = (omga^2 - sqrt(-1)*omga*tau)/(g*H);
beta = sqrt(beta_2);
TT = DT*[1:size(MONITOR_Z,1)] - DT; TT1 = TT/3600;
XX = L;
Z = real(A*exp(sqrt(-1)*omga*(TT - T/4))*cos(beta*(L - XX))/cos(beta*L));
      %U = real(-sqrt(-1)*omga*A*exp(sqrt(-1)*omga*T)*sin(beta*(L - XX))/...
      %    (beta*H*cos(beta*L)));  
plot(TT1,Z,'g');

