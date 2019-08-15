function demo7
% Masterfile for shallow water problem
% Beispiel: Closed Channel, Ninomiya, S. 167/168
% Modifiziertes EULER-Verfahren ohne lumping
% p = (p1;p2): (X,Y)-Coordinates
% Y = (U;V;Z): Velocity in x- and y-direction; Tidal elevation
% FF1        : Geometrie-File
% FF2        : File der Randbedingungen 
clc, clear, format compact
%%%% Parameter %%%%%%%%%%%%%%%%%%%%%%%%%%%
%FF1 = 'bsp03'; FF2 = 'bsp02h'; XSCALE  = 4/3; YSCALE  = 8; 
FF1 = 'bsp02'; FF2 = 'bsp02h1'; XSCALE  = 100; YSCALE  = 100;
HOURS  =  2;    % simulation time total [hour]
DT     = 10;      % Zeitschritt [sec]
PERIOD = 3600;    % Periode am offenen linken Ende [sec]
L      = 4000;    % Kanallaenge [m]
H      = 20;      % Wasserstand [m]
A      = 0.5;    % Amplitude [m]
NU     = 2.3*1E-2; % NU = MU/RHO, MU Eddy-Viskositaet 

%NU = 0;
g      = 10; % 9.81 Erdbeschleunigung [m/sec^2]
TAU    = 2.3E-4;    % TAU = 1/C^2 [s^2/m]  (not used)
%%% Ende Parametereingabe %%%%%%%%%%%%%%%%
% Iteration ueber NN Perioden
NN      = HOURS*3600/PERIOD; 
MAXITER = 3600*HOURS/DT;
%%%% Netzerzeugung %%%%%%%%%%%%%%%%%%%%%%%
%% Netzverfeinerung TODO-TODO %%%%%%%%%%%%
[p,e,t] = feval(FF1);  % erstes Gitter
p(1,:)  = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:);
N       = size(p,2);  
%%% Berechnung der Massenmatrix %%%%%%%%%%
[MASSMATRIX,MME,LUMP]  = massfun(p,e,t);
MASSMATRIX1 = kron(speye(3),MASSMATRIX);
MASSMATRIXE = kron(speye(3),MME);
LUMP1       = [LUMP;LUMP;LUMP];

I = find(e(5,:) == 1); LI = length(I); % Randsegment 1
JJ = [e(1,I(1:LI)), e(2,I(LI))];       % fuer Monitoring
%bild00(p,e,t)
%pause

Parmeter = [DT,A,H,L,PERIOD,g,N,TAU,XSCALE,YSCALE,NN,NU];
save daten7a p e t Parmeter
%%% Startwerte (Cold Start) %%%%%%%%%%%%%%%%
T = 0; U0 = zeros(N,1); V0 = U0; Z0 = U0;
Y0 = [U0;V0;Z0];
MONITOR_U = U0(JJ).'; MONITOR_Z = Z0(JJ).';
  
clf, hold on
axis([1 6 -2 2]), axis manual
% Simulation start ------------------------------------
for ITER = 1:MAXITER
   RR    = rside(T,Y0,Parmeter,p,e,t,FF2);
   %%% No Lumping %%%%%%%%%%%%%%%%%%%%%%%%
   RR    = MASSMATRIX1*Y0 + DT*RR/2;
   Y_AUX = MASSMATRIX1\RR;
   %%%% Selective Lumping FAILS%%%%%%%%%%%%%%%%%%%%%%
   %RR    = MASSMATRIXE*Y0 + DT*RR/2;
   %Y_AUX = RR./LUMP1;
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   T     = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,e,T,Parmeter);
   U = Y_AUX(1:N); V = Y_AUX(N+1:2*N); Z = Y_AUX(2*N+1:3*N);
   [RDU,RDV,RDZ] = feval(FF2,e,T,Parmeter);
   U(RDU(1,:)) = RDU(2,:)'; V(RDV(1,:)) = RDV(2,:)';
   Z(RDZ(1,:)) = RDZ(2,:)';
   V = 0*V; % bei Kanal %%%%%%%%%%%%%%%%%%%%%%%%%%
   Y1 = [U;V;Z];
   RR = rside(T,Y1,Parmeter,p,e,t,FF2);
   %%%% No Lumping %%%%%%%%%%%%%%%%%%%%%%%%%%%%
   RR = MASSMATRIX1*Y0 + DT*RR;
   Y_AUX = MASSMATRIX1\RR;
   %%%% Selective Lumping FAILS %%%%%%%%%%%%%%%%%%%%%%
  %  RR    = MASSMATRIXE*Y0 + DT*RR;
  % Y_AUX = RR./LUMP1;
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
   T = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,e,T,Parmeter);
   U = Y_AUX(1:N); V = Y_AUX(N+1:2*N); Z = Y_AUX(2*N+1:3*N);
   U(RDU(1,:)) = RDU(2,:)'; V(RDV(1,:)) = RDV(2,:)'; 
   Z(RDZ(1,:)) = RDZ(2,:)';
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   V = 0*V; % bei Kanal %%%%%%%%%%%%%%%%%%%%%
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   Y0 = [U;V;Z];
   MAXITER_ITER_T_MAXZ = [MAXITER,ITER,T,max(abs(Z))]
   plot(U(JJ),'r'), hold on   
   plot(Z(JJ),'k'),
   pause(0.1)
   set(gca,'nextplot','replacechildren');
   MONITOR_U = [MONITOR_U;U(JJ).'];
   MONITOR_Z = [MONITOR_Z;Z(JJ).'];
end
save daten7b MONITOR_U MONITOR_Z
disp(' bild07 Aufrufen! ')
