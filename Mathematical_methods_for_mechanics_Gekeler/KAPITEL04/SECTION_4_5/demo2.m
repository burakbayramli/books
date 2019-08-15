function demo2
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Re-Entry problem without inequalities
% Example 1: Apollo after Stoer, US-units
% Example 2: Apollo after Stoer, SI-units

clc, format compact, format short
nr = 100; 
while ~ismember(nr,[1,2])
   nr   = input(' Example no. (1/2) ');
end;
errorcode = 0;
%warning('off','MATLAB:nearlySingularMatrixUMFPACK')
%-------------------------------------------------
disp(' Re-Entry problem ')
switch nr
case 1, disp('Example Stoer, US-units ')
   epsilon  = 4;    % Initial penalty weights
   n     = 50;      % Number of time intervals
   Maxit = 50;      % Max. number of iterations in sqp_h.m
   acceleration  = 1; BeginMaratos = Maxit + 2;
   Tol    = 1.0E-4; % Estimated tolerance in sqp.m
   BETA   = 4.26;    % US units
   G      = 3.21715248E-4; % grav. acceleration 
   RRHO   = 2.704E-3; %US units
   R      = 209;    % US units, scaled
   T_END  = 230;    % Operational time
   SDIV2M = 26600;  % US units, scaled
   GG     = 10;     % Additional weight for objective function
   YA  = [0.36, -8.1*pi/180, 4/R];   % left boundary point
   YE  = [0.27,         0  , 2.5/R]; % right boundary point
   Parmeter = [n,BETA,G,R,RRHO,T_END,YA,YE,SDIV2M,GG];
   tol_dlqp = 1.0E-6; Eps_dlqp = 0; %1E-10; %% important!!!!!
   % 1E-10 is NOT numerically zero here !!!
   Pardlqp = [tol_dlqp,Eps_dlqp]; % Parameter for dlqp_h.m
case 2, disp(' Example Stoer, SI-units ')
   epsilon  = 4;
   n     = 50;  % Number of time intervals
   Maxit = 30;
   acceleration = 1; BeginMaratos = Maxit + 2;
   Tol    = 1E-11;     % Estimated tolerance
   BETA   = 13.9763779528;    % beta = BETA*10^-5
   G      = 9.80588076E-5;  % g = G*10^5;
   RRHO   = 1.3932E-5; % rho = RRHO*10^5
   R      = 63.70320;  % Radius of Earth = R*10^5
   T_END  = 230;       % Operational time
   SDIV2M = 169.377715147E5; % S/(2m) = SDIV2M*10^-10  !!!
   GG     = 10; % Additional weight for objective function
   YA = [0.109728, -8.1*pi/180, 1.2192/R];   % left boundary point
   YE = [0.082296     0  , 0.762/R]; % right boundary point
   % GG = 1, 100 gleiches Ergebnis
   Parmeter = [n,BETA,G,R,RRHO,T_END,YA,YE,SDIV2M,GG];
   tol_dlqp  = 1.0E-7; Eps_dlqp = 0; %1E-10;
   % 1E-10 is NOT numerically zero here !!!
   Pardlqp = [tol_dlqp,Eps_dlqp]; % Parameter for dlqp_h.m
 end  
% -- Parameter for sqp_h.m ------------------
Parsqp = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
Start  = 100;
%while ~ismember(Start,[0,1])
%   Start = input(' Initialization/Continuation ? (1/0) ');
%end
Start = 1;
if Start == 1 % New start, no continuation
   % -- Straight connection of terminal points X(1) to X(3) --
   XN1 = linspace(YA(1),YE(1),n+1)';   % Nominal state 1
   XN2 = linspace(YA(2),YE(2),n+1)';   % Nominal state 2
   XN3 = linspace(YA(3),YE(3),n+1)';   % Nominal state 3
   UN  = zeros(n+1,1);
   P1  = 1.6; P2 = 4; P3 = 0.5; SS  = 2/sqrt(pi);
   TT  = linspace(0,1,n+1);
   % -- special initial values after Stoer ------------
   for I = 1:n+1
      AA = P2*(P3 - TT(I));
      if AA > 1E-2
         UN(I) = P1*quad(@erfarg,0,AA)*SS; % Nominal control
      end
      if AA < - 1E-2
         UN(I) = - P1*quad(@erfarg,AA,0)*SS; % Nominal control
      end
   end
   X = [XN1;XN2;XN3;UN];
else % Continuation of iteration
   switch nr
   case 1, load daten3a X Parmeter
   case 2, load daten3b X Parmeter
   end
end
[X,f,errorcode] = sqp_h(@bsp10,X,Parsqp,Pardlqp,Parmeter);
%[X,f,errorcode] = sqp_h(@bsp10a,X,Parsqp,Pardlqp,Parmeter); %weak
disp(' ------------------- ')
if errorcode == 0
   disp(' Solution ');
else
   disp(' Tolerance not achieved ');
   switch errorcode
   case 1, disp(' Max. step number in iteration ')
   case 2, disp(' Max. step number in backtracking')
           disp(' try to enlarge epsilon! ')
   case 3, disp(' Max. step number in QP adaption or QP-IT.')
   end
end;
switch nr
case 1, save daten3a X Parmeter
case 2, save daten3b X Parmeter
end
fig0417a(nr)
