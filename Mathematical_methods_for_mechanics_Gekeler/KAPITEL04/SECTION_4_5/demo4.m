function demo4
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Control problem with SQP_H.M
% Spaceglider X-38, scaled
clear all, clc, format compact, format short
disp(' New start and ONE restart ! ')
errorcode = 0;
disp(' Reentry-Problem, X-38 ')
disp(' New start and one restart suffices ')
% -- Parameter for sqp.m ------------------------
n       = 100;     % number of time intervals
Maxit   = 15;      % max. step number in sqp.m
epsilon = 8;       % Initial penalty weights  
Tol     = 1.0E-13; % stop tolerance
acceleration = 1; BeginMaratos = Maxit + 2;
Parsqp  = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
% !! Maratos yields no improvement!!
% -- Parameter for dlqp.m -----------------------
tol_dlqp = 1E-6; Eps_dlqp = 0;
Pardlqp  = [tol_dlqp,Eps_dlqp];
% -- Parameter for Problem ----------------------
BTA    = 13.976;    % beta = BTA*10-5 Faktor for air density
G      = 9.806E-5;  % g = G*10^5 Grav. accel. on Earth [m/s^2]
RRHO   = 1.3932E-5; %  air pressure adapted, scaled
R      = 63.70320;  % R*10^5 Radius of Earth [m]
T_END  = 1150;      % Operational time of maneuver [sec]
omga   = 7.292115E-5; % Rotational rate of Earth [rad/s] !!!
SDIV2M = 117.604856E5;    % SDIV2M = (S/2M)*10^10 [m^2/kg]
GG     = 1E6; % Weight for objective function !!!!!!!!!!!!!
[CW_DAT,CA_DAT] = aedat; % data for c_w and c_a
% -- [V,Gama,H,chi,lambda,tau] in t = 0, t = T ------
YA  = [0.07669, -0.0025656, 0.8/R,  1.9199, -0.41888, 1.22171 ];
YE  = [0.01,             0, 0.25/R,      0, -0.49292, 2.35584 ];
Parmtr1 = [n,BTA,G,R,RRHO,T_END,SDIV2M,GG,omga];
Parmtr2 = [CW_DAT',CA_DAT',YA,YE];
Parmeter = [Parmtr1,Parmtr2];
Start = 100;
while ~ismember(Start,[0,1])
   Start = input(' New start/Restart ? (1/0) ');
end
%Start = 1;
if Start == 1 % Initialization 
   XN1 = linspace(YA(1),YE(1),n+1)';   % velocity
   XN2 = linspace(YA(2),YE(2),n+1)';   % gamma
   XN3 = linspace(YA(3),YE(3),n+1)';   % Height
   XN4 = linspace(YA(4),YE(4),n+1)';   % chi
   XN5 = linspace(YA(5),YE(5),n+1)';
   XN6 = linspace(YA(5),YE(5),n+1)';
   UN  = zeros(n+1,1);
   n0  = floor(800*(n+1)/1150);
   UN(1:n0) = - pi/6; UN(n0+1:n+1) = pi/3;
   X   = [XN1;XN2;XN3;XN4;XN5;XN6;UN];
else % Continuation 
   load daten12 X Parmeter
   % least squares approximatoin of control U
   N = 12;
   U  = X(6*n+7:7*(n+1));   % Kontrolle
   TT = linspace(0,1,length(U)).';
   p = polyfit(TT,U,N);
   V = zeros(1,length(U),1);
   for I = 1:length(U)
      V(I) = polyval(p,TT(I));
   end    
   X(6*n+7:7*(n+1)) = V;   
end
[X,f,errorcode] = sqp_h(@bsp15,X,Parsqp,Pardlqp,Parmeter);
disp(' ------------------- ')
if errorcode == 0, disp(' Solution ')
else
   disp(' No or bad solution ');
   switch errorcode
   case 1, disp(' Max. step number in iteration')
   case 2, disp(' Max. step number in backtracking')
           disp(' Try with enlarging of epsilon ! ')
   case 3, disp(' Max. step number in QP adaption or QP-IT.')
   end
end;
save daten12 X Parmeter
disp(' Call fig0416 and fig0415(1)! ')
fig0416
%pause
%fig0415(1)


