function demo2
% Elliptic BVP for discs
% full cubic triangular elements with condensation
% Example: Gabelschluessel (Spanner)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% USE of MATLAB PDE TOOLBOX for mesh generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SEGNR : Segment nrs. of boundary
% U     : displacement in x-direction
% V     : dispalcement in y-direction
% LOESUNG = [U;V;U_x;V_x;,U_Y;V_y] by the lines
% one column for each knot

clc, clear, format short, format compact
% -- Example: Geometry file and file of boundary values ---
FF1 = 'bsp02g'; FF2 = 'bsp02h';
SEGNR = [13,9,5,1,2,3,7,11,15,19,25,30,27,28,23,21,33,37,38,35,31,17];
SEG_LAST  = [3,7];   % Segmentnrn. der Lasten
SEG_LAGER = [28,37]; % Segmentnrn. der Lager
REFINE    = 2;       % No. of uniform mesh refinements
% -- Parameter -------------
E      = 0.2e+08; % Elasticity
NU     = 0.3;     % Poisson number
H      = 0.7;     % Thickness
Parmeter = [E,NU,H];
% --Initialization --------------------
[p,e,t,RAND,INNERPKTE] = prepar_n(FF1,REFINE,SEGNR);
[RDU,RDV,LASTENU,LASTENV] = feval(FF2,e);
save daten2a p e t RAND SEG_LAST SEG_LAGER
save daten2b RDU RDV LASTENU LASTENV
% -------------------------------------
load daten2a p e t RAND SEG_LAST SEG_LAGER
load daten2b RDU RDV LASTENU LASTENV
disp(' Computation of solution ');
LOESUNG = scheibe3(p,t,RDU,RDV,LASTENU,LASTENV,Parmeter);
disp(' Computation of stress ')
N = size(p,2); SPANNUNG = zeros(11,N);
for I = 1:N
   [SIGD,SIGX,SIGY,TAUXY,SIG1,SIG2,PHI,EV1,EV2]...
                 = spaqua1(LOESUNG(:,I),E,NU);
   SPANNUNG(1:7,I)   = [SIGD SIGX SIGY TAUXY SIG1 SIG2 PHI]';
   SPANNUNG(8:9,I)   = EV1;
   SPANNUNG(10:11,I) = EV2;
end
SIGDIFF = SPANNUNG(1,:);  PHI  = SPANNUNG(7,:);
SIG1 = SPANNUNG(5,:);     SIG2 = SPANNUNG(6,:);
EV1  = SPANNUNG(8:9,:);   EV2  = SPANNUNG(10:11,:);
save daten2c LOESUNG SIGDIFF SIG1 SIG2 PHI EV1 EV2
disp(' "bild02" Aufrufen! ')
