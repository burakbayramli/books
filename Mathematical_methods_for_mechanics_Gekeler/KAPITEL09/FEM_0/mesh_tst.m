function demo1
% Navier-Stokes problem, Stream function vorticity method
% Example: lid driven cavity
% X,Y   : Coordinates
% Z/W   : Stream function/vorticity
% SEGNR : ordered segment numbers for boundary
% FF1   : File for first mesh (not used in MATLAB TOOLBOX)
% FF2   : File for geometry in MATLAB format 
% FF3   : File for boundary conditions  
% OPTION = 1/2 : Without or with MATLAB TOOLBOX 
% works for DT = 0.05; NU = 0.001; MAXITER = 500

clear, clc, format short, format compact
% Example:
FF1 = 'bsp01'; FF2 = 'bsp01g'; FF3 = 'bsp01h';
% -- Parameter -------------------
MAXITER = 500;  % Step number 
DT     = 0.05;  % time step[sec]
NU     = 0.001; % coeff. of viscosity [m*m/sec]
% Choose DT = 0.025 for NU = 0.002 !!
VS     = 1;    % slip-boundary data
REFINE = 4;    % Number of uniform refinements
SEGNR  = [1,2,3,4]; % Segmentnrn. des Randes
OPTION = 2;
[p,e,t,RAND,INNERPKTE] = start4stream(FF1,FF2,OPTION,REFINE,SEGNR); 
XLAENGE = 1; YLAENGE = 1;
[p,e,t,RAND] = prepar2(p,e,t,FF2,XLAENGE,YLAENGE,SEGNR);
bild00(p,e,t,RAND)
save daten6d p e t
pause


   
   
