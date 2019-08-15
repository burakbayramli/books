function demo3b 
% Masterfile for shallow water problem
% Open Channel cf. J.Petera/V.Nassehi without PDE TOOLBOX
% p = (p1;p2;p3) : (X,Y)-Coordinates, water depth
% V = (V1;V2;V3) : V1 = U, V2 = V Velocity;
% V3 = Z         : Tidal elevation
% FF1        : Geometry data
% FF2        : File of boundary conditions 
% RDLAND Segnrn. land, RDOUT  seg.-nr. outflow (right) 
% RDSEA Segnrn. inlet (left)

clc, clear, format compact
disp(' Example 3: Long Channel ')
%%% Parameter input %%%%%%%%%%%%%%%%%%
FF1 = 'bsp03'; FF2 = 'bsp03h';
RDLAND = [1,3]; RDOUT  = 2; RDSEA  = 4;

example = 1;
switch example
case 1
   HOURS  = 1/6;       % 600 sec.
   PERIOD = 300;       % cycle for inlet [sec]
   L      = 3000;      % length of channel [m]
   H      = 10;        % water depth;
   A      = 0.3;       % Amplitude of inlet
   DT     = 1;         % good
   XSCALE = 1; YSCALE = 1; ZSCALE = 1;
case 2
   HOURS  = 1; PERIOD = 3600; L = 4000;  H = 20;    
   A = 0.5; DT = 1; 
   XSCALE = 4/3; YSCALE = 8; ZSCALE = 2;
end

nu     = 23;        % Eddy viscosity/RHO [m^2/s], Nino. p. 168
g      = 9.81;      % gravitational acceleration
n      = 0.025;     % manning's coeff. of roughness
C      = H^(1/6)/n; % Chezy coefficient
kappa = g/(H*C^2); % factor for friction after Chezy

nu = 0;  kappa = 0;

Parmeter = [A,PERIOD,g,nu,kappa,L,H];
%%% end of parameter input %%%%%%%%%%%%%
NN = HOURS*3600/PERIOD; % Iteration ueber NN Perioden
MAXITER = HOURS*3600/DT; % MAXITER*DT simulation time total [sec]
MAXITER = MAXITER + floor(L/(sqrt(g*H)*DT))
%%% mesh generation %%%%%%%%%%%%%%%%%%%%%%%%%%%
[p,e,t,MESSPKT] = feval(FF1); N = size(p,2);
p(1,:) = XSCALE*p(1,:); p(2,:) = YSCALE*p(2,:); p(3,:) = ZSCALE*p(3,:);
%bild00(p,e,t),pause
save daten3a p e t MESSPKT
[MM,MME,INVLUMP,LUMP] = massfun(p,e,t);

%%%% Cold Start %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
V = zeros(3,N); VN = V; T = 0; TIME = [];
JJ = [2,2+3*[1:30]]; MONITOR_Z = [];
clf, hold on
axis([0 35 -1 1]), axis manual 

% Simulation start ------------------------------------
for ITER = 1:MAXITER
 %  VN = V + (0.5*DT*rside_1(V,p,e,t,Parmeter))*INVLUMP; %good
 % VN = V + (0.5*DT*rside_1(V,p,e,t,Parmeter))/MM;  % better  
 %  VN = (V*MM + 0.5*DT*rside_1(V,p,e,t,Parmeter))*INVLUMP; %wrong
   VN = (V*MME + 0.5*DT*rside_1(V,p,e,t,Parmeter))*INVLUMP;  % good
   T   = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,p,e,V,T,A,PERIOD,H,L);
   VN(1,RDU(1,:)) = RDU(2,:); VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   %%%% Correcture for straight channel (necessary) %%%%%%%%%%%%%%
   VN(2,:) = 0; 
   % ------------------------------
  % VN = V + (DT*rside_1(VN,p,e,t,Parmeter))*INVLUMP; 
  % VN = V + (DT*rside_1(VN,p,e,t,Parmeter))/MM;
 % VN = (V*MM + DT*rside_1(VN,p,e,t,Parmeter))*INVLUMP;
   VN = (V*MME + DT*rside_1(VN,p,e,t,Parmeter))*INVLUMP;
   T   = T + DT/2;
   [RDU,RDV,RDZ] = feval(FF2,p,e,V,T,A,PERIOD,H,L);
   VN(1,RDU(1,:)) = RDU(2,:); VN(2,RDV(1,:)) = RDV(2,:);
   VN(3,RDZ(1,:)) = RDZ(2,:);
   %%%% Correcture for channel (necessary) %%%%%%%%%%%%%%
   VN(2,:) = 0; 
   V = VN;
   if T - floor(T) == 0, MAXIT_ITER_SEC = [MAXITER,ITER,T], end
   % ----------------------------
   grid on
   plot(V(3,JJ),'r'); hold on
   AUX = (2*pi/PERIOD)*(T*ones(1,N) - p(1,:)/sqrt(g*H));
   AUX1 = AUX;
   J = find(AUX1 < 0); AUX(J) = 0;
   Z_EXACT = A*sin(AUX); % exact for open end
   plot(Z_EXACT(JJ),'b'); hold on
   set(gca,'nextplot','replacechildren')
   pause(0.001)
   if T >= 300
      TIME = [TIME,T];
      MONITOR_Z = [MONITOR_Z,VN(3,MESSPKT)];
   end
end
save daten3b TIME MONITOR_Z V Parmeter
disp(' Call bild03 ! ') 
