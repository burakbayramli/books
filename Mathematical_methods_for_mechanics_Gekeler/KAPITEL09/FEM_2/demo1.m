function demo1
% Elliptic BVP for discs
% full cubic triangular elements with condensation
% Example: Gabelschluessel (Spanner)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOT USE of MATLAB PDE TOOLBOX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear, clc, format short, format compact
% -- Parameter -------------
E      = 0.2e+08; % Elasticity
NU     = 0.3;     % Poisson number
H      = 0.7;     % Thickness
Parmeter = [E,NU,H];
REFINE = 1; % No. of uniform mesh refinements
% --Initialization --------------------
[p,e,t] = bsp01g; q = [];
for I = 1:REFINE
   disp(' Refinemesh ')
   [p,e,t] = mesh01(p,e,t);
   disp(' Jigglemesh ')
   p = mesh10(p,e,t);
end
%pause
[RDU,RDV,LASTENU,LASTENV] = bsp01h(e);
save daten1a p e t
save daten1b RDU RDV LASTENU LASTENV
% -------------------------------------
load daten1a p e t
load daten1b RDU RDV LASTENU LASTENV
disp(' Computation of solution ');
LOESUNG = scheibe3(p,t,RDU,RDV,LASTENU,LASTENV,Parmeter);
% -- Spannungen ------------------------------------------
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
save daten1c LOESUNG SIGDIFF SIG1 SIG2 PHI EV1 EV2
disp(' "bild01" Aufrufen! ')
