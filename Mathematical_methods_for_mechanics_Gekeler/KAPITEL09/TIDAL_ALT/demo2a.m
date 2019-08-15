function demo2a
% Masterfile for shallow water problem
% Channel with inlet and no outlet
% cf. H.Ninomiya/K.Onishi
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WITHOUT MATLAB PDE TOOLBOX, WITH ODE23.M %%%
%%% VERFAHREN VERSAGT BEI ORIGINALBEISPIEL   %%%
%%% von Ninomiya
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% p = (p1;p2;p3): (X,Y)-Coordinates, water depth
% U =           : Velocity in x-direction
% V =           : Velocity in x-direction
% Z             : Tidal elevation
% ---------------------------------
clc, clear, format compact
Beispiel = 100; KK = [1,2];
%while ~ismember(Beispiel,KK)
%   Beispiel = input(' Welches Beispiel? (1/2/3)')
%end
disp(' Kanallaenge 1000 m, 1 Std., Periode: 6 Minuten ')
FF1 = 'bsp02'; FF2 = 'bsp02h';

XSCALE  = 25;   % scale factor in X-axis,
YSCALE  = 100;  % scale factor in Y-axis
L       = 1000; % Kanallaenge [m]
H       = 20;   % Wasserstand [m]
PERIOD  = 360;  % Periode am offenen Ende [sec]
% PERIOD = 180/360/540/720
A       = 1;    % Amplitude [m]
g       = 9.81;   % [m/s*s] % Abb. 9.33 mit g = 10
TAU     = 0;
DT      = 60; % Zeitschritt [sec]
HOURS  =  1;  % simulation time total [hour]
NN = HOURS*3600/PERIOD; 
NU = 2.3E-2; %kinematische Viskositaet (Wasser)

% ------------------------------------------
[p,e,t] = feval(FF1);
p(1,:) = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:); N = size(p,2);
save daten2a p e t

Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU];
PUNKTA = 0; PUNKTB = 0;
tspan = [0,DT]; Stunden = 0;
MASSMATRIX = massfun(p,e,t);  
X0 = zeros(3*N,1);
options = odeset('Reltol',1E-4);
MAXITER =  HOURS*3600/DT    % MAXITER*DT simulation time total [sec]
% -------------------------------------
for ITER = 1:MAXITER
   [T,Y] = ode23(@rside,tspan,X0,options,Parmeter,p,e,t,FF2,MASSMATRIX);
   tspan = tspan + DT;
   [LM,LN] = size(Y); TT = T(LM);
   U = Y(LM,1:N); V = Y(LM,N+1:2*N); Z = Y(LM,2*N+1:3*N);
   % -- Randwerte Einfuegen ---------------
   [RDU,RDV,RDZ] = bsp02h(e,TT,Parmeter);
   U(RDU(1,:)) = RDU(2,:);
   V(RDV(1,:)) = RDV(2,:);
   Z(RDZ(1,:)) = RDZ(2,:);
   X0 = [U,V,Z]';
   IT_MIN = ITER - floor(ITER/(60/DT))*(60/DT);
   if IT_MIN == 0
      Minuten = ITER*DT/60
      PUNKTA = [PUNKTA,Z(14)];
      PUNKTB = [PUNKTB,Z(7)];
      save daten2b U V Z PUNKTA PUNKTB
      bild02a
      pause(0.3)
   end
   IT_STD = ITER - floor(ITER/(3600/DT))*(3600/DT);
   if IT_STD == 0
      Stunden = Stunden + 1
   end
   if abs(Z(7)) > 100, disp('Divergenz'), return, end
   pause(0.2)
end
save daten2c PUNKTA PUNKTB, bild02b
