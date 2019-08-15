function demo8
% Navier-Stokes problem
% Stream function vorticity method
% Coupled system following
% Barragy-Carey: Comm. Num. Meth. Eng. 9 (1993), 387-395
% simple iteration
% Example with exact solution, cf. Spotz, S. 3504
% X,Y : Coordinates
% V   : Velocity at the center of triangle,
% Z/W : Stream function/vorticity
% RDZ : Dirichlet-RB fuer Z
% RCZ : Cauchy-RB fuer Z
% RDW : Dirichlet-RB fuer W

clear, clc, format short, format compact
% Example: 
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp08h';
% -- Parameters -------------------
MAXITER = 20;
NU      = 0.01; % coeff. of viscosity [m*m/sec]
VS      = 0;    % slip-boundary data
SEGNR = [1,2,3,4]; % Segmentnrn. des Randes
EPSILON = 0;
Parmeter = [0,NU,VS];
disp(' Computation of geometry data ')
REFINE = 4;    % Number of uniform refinements
[p,e,t] = feval(FF1); % erstes Gitter
for J = 1:REFINE
   disp(' Refinemesh ')
   [p,e,t] = mesh01_t(FF2,p,e,t);
%   p       = mesh10(p,e,t,4); % Jigglemesh
%   t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
end
RAND = e;
% -- Innere Punkte --------------
LP = size(p,2); AUX = zeros(1,LP);
for I = 1:LP
   if isempty(find(e(1,:) == I)), AUX(I) = 1; end
end
IP = find(AUX == 1); % innere Punkte

[RDZ,RCZ,RDW] = feval(FF3,p,e,t,Parmeter);

disp(' Data for inhomogeneous problem  ')
% DATA = [z;w;u;v,deltav_x;deltau_y,grad2_x;grad1_y];
DATA = bsp08_data(p);
Z0 = DATA(1,:)'; W0 = DATA(2,:)';
ROTF1 = -NU*(DATA(5,:) - DATA(6,:));
ROTF2 = DATA(7,:) - DATA(8,:);
RSIDE = ROTF1 + ROTF2;
% -- initial values -------------------
N = size(p,2); Z = ones(N,1); W = ones(N,1);
% -- Matrix und rechte Seite ----------
[KK,MM,Q] = matrizen1(p,e,t,RCZ);
AA = [MM, -KK(:,IP); NU*KK(IP,:), EPSILON*MM(IP,IP)];
G  = MM*RSIDE';
% -------------------------------------
save daten8a p e t IP Parmeter Z0 W0
save daten8b RDZ RCZ RDW
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
for ITER = 1:MAXITER
   WR = trilin1(p,t,W,Z);
   RS = [KK(:,RDZ(1,:))*Z(RDZ(1,:)); (G(IP) - WR(IP))];
   XX = AA\RS;
   WN = XX(1:N); ZN = zeros(N,1); ZN(IP) = XX(N+1:N+length(IP));
   ZN(RDZ(1,:)) = RDZ(2,:);
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER,DIFFZ, DIFFW]
end
%V = velocity(p,e,t,Z);
save daten8c W Z
disp(' bild08 Aufrufen!')
%bild08
