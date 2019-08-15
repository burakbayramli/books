function stokes_aux1
% Maple-File fuer TH-Element
% TH-Element ohne Bubble-Funktion
clc, clear, format compact, format short
syms x y
PSI = [(1-x-y)*(1-2*x-2*y),x*(2*x-1),y*(2*y-1),4*x*(1-x-y),4*x*y,4*y*(1-x-y)];
%PSI_XI  = diff(PSI,'x')
%PSI_ETA = diff(PSI,'y')
PSI_XI = [-3+4*x+4*y, 4*x-1, 0, 4-8*x-4*y, 4*y, -4*y];
PSI_ETA =[-3+4*x+4*y, 0, 4*y-1,-4*x, 4*x, 4-4*x-8*y];
PSIL = [1-x-y, x , y];


A  = PSI_XI.'*PSI_XI;   A = int(A,'x','0','1-y');
S1 = int(A,'y',0,1)*6
A  = PSI_XI.'*PSI_ETA;  A = int(A,'x','0','1-y');
S2 = int(A,'y',0,1);
S2 = (S2 + S2.')*6
A  = PSI_ETA.'*PSI_ETA; A = int(A,'x','0','1-y');
S3 = int(A,'y',0,1)*6
A  = PSI_XI.'*PSIL ;      A = int(A,'x','0','1-y');
C1 = int(A,'y',0,1)*6
A  = PSI_ETA.'*PSIL ;     A = int(A,'x','0','1-y');
C2 = int(A,'y',0,1)*6
% -- Massenmatrix -----------
AA = PSI.'*PSI;
A = int(AA,'x',0,'1-y');   S4 = int(A,'y',0,1)*360
AUX1 = x*AA;
BB = int(AUX1,'x',0,'1-y'); P = int(BB,'y',0,1)*360*7
AUX2 = y*AA;
BB = int(AUX2,'x',0,'1-y'); Q = int(BB,'y',0,1)*360*7

A  = PSI.'*PSI_XI ;      A = int(A,'x','0','1-y');
D1 = int(A,'y',0,1)*30
A  = PSI.'*PSI_ETA ;     A = int(A,'x','0','1-y');
D2 = int(A,'y',0,1)*30

       