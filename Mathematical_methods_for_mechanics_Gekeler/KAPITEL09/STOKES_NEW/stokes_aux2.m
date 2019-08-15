function stokes_aux2
% Maple-File fuer Mini-Element
clc, clear, format compact, format short
syms x y
PSI     = [1-x-y,x,y,27*x*y*(1-x-y)];
%PSI_XI  = diff(PSI,'x')
%PSI_ETA = diff(PSI,'y')
PSI_XI =  [-1;1;0; 27*y*(1-x-y)-27*x*y];
PSI_ETA = [-1;0;1; 27*x*(1-x-y)-27*x*y];
PSIL = [1-x-y, x, y];

A = PSI_XI*PSI_XI.'; A = int(A,'x','0','1-y');
SS1 = int(A,'y',0,1)*20
A = PSI_XI*PSI_ETA.'; A = int(A,'x','0','1-y');
SS2 = int(A,'y',0,1);
SS2 = (SS2 + SS2.')*20
A = PSI_ETA*PSI_ETA.'; A = int(A,'x','0','1-y');
SS3 = int(A,'y',0,1)*20
A = PSI_XI*PSIL; A = int(A,'x','0','1-y');
C1 = int(A,'y',0,1)*120
A = PSI_ETA*PSIL; A = int(A,'x','0','1-y');
C2 = int(A,'y',0,1)*120
       