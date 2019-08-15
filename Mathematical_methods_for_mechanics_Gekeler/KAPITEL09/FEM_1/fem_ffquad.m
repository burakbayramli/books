function [FF,FFS] = fem_ffquad(SIG);
% vgl. SCHWARZ: FEM
% liefert zum gegebenem Wert SIG die drei Formfunktionen und
% deren partielle Ableitungen fuer den quadratischen Ansatz
% in den Vektoren FF(3)und  FFS(3)

L1     = SIG;
L2     = 1 - SIG;
FF     = zeros(3,1);
FFS    = FF;
FF(1)  = L2*(L2 - L1);
FF(2)  = 4*L1*L2;
FF(3)  = L1*(L1 - L2);
FFS(1) = L1 - 3*L2;
FFS(2) = 4*(L2 - L1);
FFS(3) = 3*L1 - L2;
