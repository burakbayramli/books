function [FF,FFX,FFE] = fem_ffqpas(XSI,ETA);
% vgl. SCHWARZ: FEM
% liefert zum Wertepaar XSI, ETA die Formfunktionen und deren
% partielle Ableitungen fuer den quadratischen Ansatz
% der Serendipity-Klasse im Quadrat in den Vektoren
% FF(8), FFX(8), FFE(8)

H1     = 1 - XSI;
H2     = 1 - ETA;
H3     = 1 - 2*(XSI + ETA);
H4     = 1 - 2*(XSI - ETA);
H5     = 3 - 2*(XSI + ETA);
H6     = 1 + 2*(XSI - ETA);
FF     = zeros(8,1);
FFX    = zeros(8,1);
FFE    = zeros(8,1);
FF(1)  = H1*H2*H3;
FF(2)  = -XSI*H2*H4;
FF(3)  = -XSI*ETA*H5;
FF(4)  = -ETA*H1*H6;
FF(5)  = 4*XSI*H1*H2;
FF(6)  = 4*XSI*ETA*H2;
FF(7)  = 4*XSI*ETA*H1;
FF(8)  = 4*ETA*H1*H2;
FFX(1) = -H2*(H3 + 2*H1);
FFX(2) = -H2*(H4 - 2*XSI);
FFX(3) = -ETA*(H5 - 2*XSI);
FFX(4) = -ETA*(2*H1 - H6);
FFX(5) = 4*H2*(H1 - XSI);
FFX(6) = 4*ETA*H2;
FFX(7) = 4*ETA*(H1 - XSI);
FFX(8) = -4*ETA*H2;
FFE(1) = -H1*(H3 + 2*H2);
FFE(2) = -XSI*(2*H2 - H4);
FFE(3) = - XSI*(H5 - 2*ETA);
FFE(4) = -H1*(H6 - 2*ETA);
FFE(5) = -4*XSI*H1;
FFE(6) = 4*XSI*(H2 - ETA);
FFE(7) = 4*XSI*H1;
FFE(8) = 4*H1*(H2 - ETA);
