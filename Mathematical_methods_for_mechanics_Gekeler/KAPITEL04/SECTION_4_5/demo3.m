function demo3
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Re-Entry problem without inequalities
% Modified equations
% Example 1: Apollo after Stoer, US-units
% Example 2: Apollo after Stoer, SI-units
% Example 3: X-38 simplified, adapted to Stoer, SI-units
% EXample 4: X-38 simplified, adapted to Stoer, SI-units

clear, clc, format compact, format short
nr = 100; 
while ~ismember(nr,[3,4])
   nr   = input(' Example no. (3/4) ');
end;
%nr = 4;
errorcode = 0;
%warning('off','MATLAB:nearlySingularMatrixUMFPACK')
%-------------------------------------------------
disp(' Re-Entry problem, X-38 ')
switch nr
case 3, disp(' X-38 with Stoer, SI-units ')
   %BEISPIEL = @bsp12;   %non-physical X-38 problem
   BEISPIEL = @bsp13;   % simplified X-38 problem
   n = 50;  % Number of time intervals
   Maxit = 10;
   Tol   = 1E-11; % Estimated tolerance
   epsilon  = 4; 
   acceleration = 1; BeginMaratos = Maxit + 2;
   BTA    = 13.976;    % beta = BTA*10^-5
   G      = 9.806E-5;  % g = G*10^5;
   RRHO   = 1.3932E-5; % rho = RRHO*10^5
   R      = 63.70320;  % Radius of Earth = R*10^5
   T_END  = 1150;      % Operational time
   SDIV2M = 117.604856E5;
   GG     = 10; % Additional weight for objective function
   YA  = [0.077, -0.0025656, 0.8/R]; % left boundary point
   YE  = [0.01,  0, 0.25/R];         % right boundary point
   Parmeter = [n,BTA,G,R,RRHO,T_END,YA,YE,SDIV2M,GG];
   tol_dlqp = 1.0E-5; Eps_dlqp = 0;
   Pardlqp = [tol_dlqp,Eps_dlqp];
case 4, disp(' X-38 with Stoer, SI-units ')
   BEISPIEL = @bsp14;   % simplified X-38 problem
   n = 50;  % Number of time intervals, same result with 100
   Maxit = 10
   Tol   = 1E-11; % Estimated tolerance
   epsilon  = 4;
   acceleration  = 1; BeginMaratos = Maxit + 2;
   BTA    = 13.976;    % beta = BTA*10^-5
   G      = 9.806E-5;  % g = G*10^5;
   RRHO   = 1.3932E-5; % rho = RRHO*10^5
   R      = 63.70320;  % radius of Earth = R*10^5
   T_END  = 1150;      % operational time
   SDIV2M = 117.604856E5;
   GG     = 100; % Additional weight for objective function
   % Boundary values for V, Gama, H, chi ---------------------
   YA = [0.077, -0.0025656, 0.8/R, 1.9199];   % left boundary
   YE = [0.01,  0, 0.25/R, 0 ]; % right boundary
   Parmeter = [n,BTA,G,R,RRHO,T_END,SDIV2M,GG,YA,YE];
   tol_dlqp = 1.0E-5; Eps_dlqp = 0; ;
   Pardlqp = [tol_dlqp,Eps_dlqp];
 end  
% -- Parameter for sqp_h.m ------------------
Parsqp = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
Start = 100;
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
   if nr == 3 % Start for X-38
      n0 = floor(800*(n+1)/1150);
      UN(1:n0) = - pi/6; UN(n0+1:n+1) = pi/3;
      X   = [XN1;XN2;XN3;UN];
   end
   if nr == 4
      n0 = floor(800*(n+1)/1150);
      XN4 = linspace(YA(4),YE(4),n+1)';
      UN(1:n0) = - pi/6; UN(n0+1:n+1) = pi/3;
      X   = [XN1;XN2;XN3;XN4;UN];
   end
else % Continuation of iteration
   switch nr
   case 3, load daten3c X Parmeter
   case 4, load daten3d X Parmeter
   end
end
if nr == 3
   [X,f,errorcode] = sqp_h_a(BEISPIEL,X,Parsqp,Pardlqp,Parmeter);
 %   [X,f,errorcode] = sqp_h(BEISPIEL,X,Parsqp,Pardlqp,Parmeter);
else
   [X,f,errorcode] = sqp_h_b(BEISPIEL,X,Parsqp,Pardlqp,Parmeter);
end
disp(' ------------------- ')
if errorcode == 0
   disp(' Solution ');
else
   disp(' No or bad solution ');
   switch errorcode
   case 1, disp(' Max. step number in iteration ')
   case 2, disp(' Max. step number in backtracking')
           disp(' try to enlarge epsilon! ')
   case 3, disp(' Max. step number in QP adaption or QP-IT.')
   end
end;
switch nr
case 3, save daten3c X Parmeter
case 4, save daten3d X Parmeter
end
fig0417a(nr)
