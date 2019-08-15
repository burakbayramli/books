function demo7
% Navier-Stokes problem
% Stream function vorticity method
% Coupled system following
% Barragy-Carey: Comm. Num. Meth. Eng. 9 (1993), 387-395
% simple iteration
% Example: lid driven cavity
% X,Y   : Coordinates
% V     : Velocity at the center of triangle,
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% RDZ   : Dirichlet-RB fuer Z
% RCZ   : Cauchy-RB fuer Z
% RDW   : Dirichlet-RB fuer W

clear, clc, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp07h';
% -- Parameters -------------------
MAXITER = 20;
% Cold start:
NU     = 0.01; % coeff. of viscosity [m*m/sec] Re = 100;
% SUccession for restart
%NU     = 0.005; % coeff. of viscosity [m*m/sec] Re = 200;
%NU     = 0.0025; % coeff. of viscosity [m*m/sec] Re = 400;
%NU     = 0.001; % coeff. of viscosity [m*m/sec] Re = 1000;
VS      = 1;    % slip-boundary data
SEGNR   = [1,2,3,4]; % Segmentnrn. des Randes
REFINE  = 5;    % Number of uniform refinements
EPSILON = 0;
Parmeter = [0,NU,VS];

disp(' Computation of geometry data ')
REFINE = 3;    % Number of uniform refinements
[p,e,t] = feval(FF1); % erstes Gitter
for J = 1:REFINE
   disp(' Refinemesh ')
   [p,e,t] = mesh01_t(FF2,p,e,t);
   %   p       = mesh10(p,e,t,4); % Jigglemesh
   %   t       = mesh03(p,t,0);   % Lange Kanten durch kurze Ersetzen
end
% -- Innere Punkte --------------
LP = size(p,2); AUX = zeros(1,LP);
for I = 1:LP
   if isempty(find(e(1,:) == I)), AUX(I) = 1; end
end
IP = find(AUX == 1);

[RDZ,RCZ,RDW]   = feval(FF3,p,e,t,Parmeter);
[KK,MM,Q]       = matrizen1(p,e,t,RCZ);
N = size(p,2); Z = zeros(N,1); W = Z;
save daten7a p e t Parmeter
save daten7b RDZ RCZ RDW
AA = [MM, -KK(:,IP); NU*KK(IP,:), EPSILON*MM(IP,IP)];
% ------------------------------
N = size(p,2);
for ITER = 1:MAXITER
   WR = trilin1(p,t,W,Z);
   RS = [KK(:,RDZ(1,:))*Z(RDZ(1,:)) - Q; -WR(IP)];
   % -- Dirichlet boundary condition for W --------
   J      = RDW(1,:);
   RS      = RS - AA(:,J)*RDW(2,:)';
   RS(J)   = RDW(2,:)';
   AA(J,:) = 0; AA(:,J) = 0;
   for I = 1:size(RDW,2), J = RDW(1,I); AA(J,J)  = 1; end
   XX = AA\RS;
   WN = XX(1:N); ZN = zeros(N,1);
   ZN(IP) = XX(N+1:N+length(IP));
   ZN(RDZ(1,:)) = RDZ(2,:)';
   DIFFZ = max(abs(ZN - Z)); DIFFW  = max(abs(WN - W));
   Z = ZN; W = WN;
   ITER_DIFFZ_DIFFW = [ITER, DIFFZ, DIFFW]
end
%V = velocity(p,e,t,Z);
save daten7c W Z
disp(' bild07 Aufrufen!')
%bild07
