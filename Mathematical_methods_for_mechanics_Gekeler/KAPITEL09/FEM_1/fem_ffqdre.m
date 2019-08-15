function [FF,FFX,FFE] = fem_ffqdre(XSI,ETA);
% vgl. SCHWARZ: FEM
% liefert zum Wertepaar XSI, ETA die Formfunktionen und deren
% partielle Ableitungen fuer den quadratischen Ansatz in einem
% Dreieckelement in den Vektoren FF(6), FFX(6), FFE(6)

Z1     = 1 - XSI - ETA;
Z2     = XSI;
Z3     = ETA;
FF     = zeros(6,1);
FFX    = zeros(6,1);
FFE    = zeros(6,1);
FF(1)  = Z1*(Z1 + Z1 - 1);
FF(2)  = Z2*(Z2 + Z2 - 1);
FF(3)  = Z3*(Z3 + Z3 - 1);
FF(4)  = 4*Z1*Z2;
FF(5)  = 4*Z2*Z3;
FF(6)  = 4*Z3*Z1;
FFX(1) = 1 - 4*Z1;
FFX(2) = 4*Z2 - 1;
FFX(3) = 0;
FFX(4) = 4*(Z1 - Z2);
FFX(5) = 4*Z3;
FFX(6) = -4*Z3;
FFE(1) = 1 - 4*Z1;
FFE(2) = 0;
FFE(3) = 4*Z3 - 1;
FFE(4) = -4*Z2;
FFE(5) = 4*Z2;
FFE(6) = 4*(Z1 - Z3);
