function [FF,FFX,FFE] = fem_ffqbil(XSI,ETA);
% liefert zum Wertepaar XSI, ETA die Formfunktionen und deren
% partielle Ableitungen fuer den bilinearen Ansatz
% der Serendipity-Klasse im Quadrat in den Vektoren
% FF(4), FFX(4), FFE(4)

FF     = zeros(4,1);
FFX    = zeros(4,1);
FFE    = zeros(4,1);
FF(1)  = (1 - XSI)*(1 - ETA);
FF(2)  = XSI*(1 - ETA);
FF(3)  = XSI*ETA;
FF(4)  = (1 - XSI)*ETA;
FFX(1) = ETA - 1;
FFX(2) = 1 - ETA;
FFX(3) = ETA;
FFX(4) = - ETA;
FFE(1) = XSI - 1;
FFE(2) = - XSI;
FFE(3) = XSI;
FFE(4) = 1 - XSI;
