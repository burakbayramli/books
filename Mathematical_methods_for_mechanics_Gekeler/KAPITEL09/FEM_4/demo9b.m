function demo9b
% Transport problem following H.Ninomiya/K.Onishi
% Example Ninomiya, 8.4.1, corresponds to forced convection
% X,Y : Coordinates
% V   : Velocity at the center of triangle,
% Z/W : Stream function/vorticity
% Q   : Pollution
% SEGNR: ordered segment numbers for boundary
clc, format compact, format short, errorcode = 0;
disp(' Zuerst DEMO9A Aufrufen, um Wind zu erzeugen! ')
%%%   DATENEINGABE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp09'; FF2 = 'bsp09hb';
SEGNR  = [1,2,3,4,5,6];
SEGNR_RCQ = 4; 
%% Parameter fuer Transport %%%%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = 300; DT     = 0.1;     % time step [sec]
NU_T    = 1.45E-5; % coeff. of viscosity [m*m/sec]
ETA     = 1.58E-5; % diffusion coefficient (Fick's law)[m*m/sec]
SIGMA   = 0;      % rho/rho_0 = 1 + sigma(Q - Q_0)
                  % Dichte aendert sich nicht!
g       = 9.81;    % gravitational acceleration [m/(s*s)]
%% Einfacheres Beispiel: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
MAXITER = 60; DT = 0.5; % time step [sec]
NU_T = 1.0E-3; % coeff. of viscosity [m*m/sec]
ETA  = NU_T  ; % diffusion coefficient (Fick's law)[m*m/sec]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%%% Parameter fuer Konvektion: %%%%%%%%%%%%
%%%% NU, LAMBDA, BETA, KAPPA; T_AIR, g %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
SCALE = 0;
if SCALE == 0 % Im unskalierten Problem ist hierfuer einzusetzen:
   NU = NU_T; BETA = - SIGMA; LAMBDA = ETA; 
   T_AIR = 0; %(not used)
   KAPPA = 0; %(not used)
   DELTA_U = 1;   % char. Geschwindigkeit [m/s]
   DELTA_L = 1;   % char. Laenge [m]
   XLAENGE = 1; YLAENGE = 1;
else % Im dim.-losen System (forced convection) ist
   DELTA_U = 2;   % char. Geschwindigkeit [m/s]
   DELTA_L = 6;   % char. Laenge [m]
   %DELTA_Q = (?) % Char. Pollution [kg(m^3] (not used)
   RE = 7.6E5;    % REYNOLDS-Zahl = DELTA_U*DELTA_L/NU
   SC = 0.918;    % SCHMIDT-Zahl = NU/ETA;
   FR = DELTA_U^2/(g*SIGMA*DELTA_Q) % FROUD-Zahl
   % 1/FR = 0 wegen SIGMA = 0;
   NU = 1/RE; LAMBDA = 1/(RE*SC); %g*BETA = - 1/FR ;
   BETA = 0; % wegen SIGMA = 0;
   XLAENGE = 1/6; YLAENGE = 1/6;
end
%%%%%% Ende der Dateneingabe %%%%%%%%%%%%%%%%

Parmeter = [DT,NU,LAMBDA,BETA,KAPPA,T_AIR,g,YLAENGE,DELTA_U]; 
load daten9a p e RAND t 
load daten9b RDZ RDW NACHBAR NORMALEN
load daten9c V W Z 
[RDZ,RDW,RDQ,RCQ,RDM] = feval(FF2,e,Parmeter);
save daten10a p e RAND t Parmeter XLAENGE YLAENGE
save daten10b RDZ RDW RDQ RDM NACHBAR NORMALEN
% ------------------------------
Q = zeros(size(p,2),1);
for ITER = 1:MAXITER
   %QN = convection_c(p,t,Z,RDQ,RCQ,RDM,Q,Parmeter);
   QN = air(p,t,Q,Z,RDQ,RDM,Parmeter);
   DIFFQ = max(abs(QN - Q)); 
   Q = QN;  
   ITER_DIFFQ = [ITER, DIFFQ]
end
V = velocity(p,e,t,Z);
save daten10c V W Z Q
disp(' bild09b Aufrufen!')
%bild09b
