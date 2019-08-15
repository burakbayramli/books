function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 12.5.08
% Masterfile for nonlinear optimization problems
% Gradient-projection method
% optional with numerically calculated derivatives
% f(x) = Min!, g(x) >= 0, h(x) = 0
% INPUT  'name'  Name of problem
%        x         : Start vector (must be feasible)
%        Accel     : Diff. options for acceleration (1,2,3)
%        Eps       : instead zero (problem related)
%        gamma     : Parameter for inactivation
%        lin_restr : 0/1, 1 for linear restrictions
%        Maxit     : max. step number
%        Tol       : Stopping tolerance 
% OUTPUT x         = Solution if method successful
%        f         = Value of objective function
%        errorcode = 1: Max. step number
%        errorcode = 2: Max. step number in backtracking
%        errorcode = 3: x not feasible
% Examples must have the form Y = examplename(X,flag,Parmeter);
% X state vector, Parmeter: additional parameters for problem
% flag = 1: Objective function      f
% flag = 2: Inequality constraints  g
% flag = 3: Equality constraints    h
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequality constraints
% flag = 6: Gradient of equality constraints
% optionally in 2-dimensional problems: 
% flag = 7: boundary of feasible domain 

clc, clear, format short, format compact
errorcode = 0;
nr = 100; KK = [1 2 3 4 6 7 8 9 10 12 13 14 16 17];
while ~ismember(nr,KK)
   nr   = input(' Example no. (1/2/3/4/6/7/8/9/10/12/13/14/16/17) ');
end;
disp(' In lin. restrictions set "lin_restr" = 1 ');
disp(' for acceleration ');
switch nr
case 1, disp(' Example Spellucci, p. 397 ')
   X = [0; 0]; X_OPT = [-1; 0];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 40;
   Lin_restr = 0;  Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
 %  [X,f,errorcode,PFAD] = gp_g(@bsp01,X,Pargpv,Parmeter);
   [X,f,errorcode,PFAD] = gp_g(@bsp01a,X,Pargpv,Parmeter);
   save dateng1 PFAD
   bild01(1)
case 2, disp(' Example Spellucci, p. 457 ')
   X = [-1; 0]; X_OPT = [0.546; 0.702];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 20;
   Lin_restr = 0; Tol = 1.0E-5;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
  % [X,f,errorcode,PFAD] = gp_g(@bsp02,X,Pargpv,Parmeter);
   [X,f,errorcode,PFAD] = gp_g(@bsp02a,X,Pargpv,Parmeter);
   save dateng2 PFAD
   bild02(1)
case 3, disp(' Example 1, Gekeler ')
   X = [-1; 0]; X_OPT = [3.45;0.19];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 20;
   Lin_restr = 0; Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp03,X,Pargpv,Parmeter);
   save dateng3 PFAD
   bild03(1)
case 4, disp(' Example 2, Gekeler ')
   X_OPT     = [2.43;1.27];
   Accel = 2; Maxit = 20; Lin_restr = 0; Tol = 1.0E-5;
   % ------------------------
   %for I =  1:3;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   for I =  4:6;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % ------------------------
   switch I
   case 1, X0 = -3/4;  X  =  [X0;sqrt(25 - (X0 - 4)^2)];
           Eps = 1.0E-5; Gamma = 1/2;
   case 2, X0 = - 3/4; X  = [X0;- sqrt(25 - (X0 - 4)^2)];
           Eps = 1.0E-5; Gamma = 1/2;
   case 3, X = [0;-2];
           Eps = 1.0E-3; Gamma = 1/2;
   case 4, X = [-3/4;sqrt(25 - (19/4)^2)];
           Eps = 1.0E-5; Gamma = 1/10;
   case 5, X = [-3/4;- sqrt(25 - (19/4)^2)];
           Eps = 1.0E-5; Gamma = 1/10;
   case 6, X = [0;-2];
           Eps = 1.0E-3; Gamma = 1/10;
   end
   Pargpv  = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp04,X,Pargpv,Parmeter);
   if I == 1 save dateng4a PFAD; end
   if I == 2 save dateng4b PFAD; end
   if I == 3 save dateng4c PFAD; end
   if I == 4 save dateng4d PFAD; end
   if I == 5 save dateng4e PFAD; end
   if I == 6 save dateng4f PFAD; end
   end
   bild04(1)
case 6, disp(' Example Himmelblau, p. 393, inequalities only ')
   X     = [-1; 0];      % feasible
   X_OPT = [1.665; 0.554];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 40;
   Lin_restr = 0; Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp06,X,Pargpv,Parmeter);
case 7, disp(' Example Himmelblau, p. 393 ')
   X     = [-1; 0];
   X     = [0; 0.5];
   X_OPT = [0.823; 0.911];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 20;
   Lin_restr = 0; Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode] = gp(@bsp07,X,Pargpv,Parmeter);
case 8, disp(' Example Himmelblau, p. 397 ')
   X     = [4.846153846153846153; 1.230769230769230769; 0];
   X_OPT = [3.512; 0.217; 3.552];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 40;
   Lin_restr = 0; Tol = 1.0E-5;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode] = gp(@bsp08,X,Pargpv,Parmeter);
case 9, disp(' Ex. Spellucci, p. 339, works only with anti-zigzag strategy ')
   X     = [0; 1/4; 1/2]; X_OPT = [0; 0; 2];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 20;
   Lin_restr = 1; Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp09,X,Pargpv,Parmeter);
case 10, disp(' Example Spellucci, p. 370 (Rosen-Suzuki) ')
   X = [0; 0; 0; 0]; X_OPT  = [0; 1; 2; -1];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 30;
   Lin_restr = 0; Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp10,X,Pargpv,Parmeter);
case 12, disp(' Example Himmelblau, p. 403 ')
   X     = [-3; -1; -3; -1];  % feasible
   X_OPT = [1; 1;  1; 1];
   Accel = 2; Eps = 1.0E-3; Gamma = 0.5; Maxit = 60;
   Lin_restr = 1; Tol = 1.0E-5;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp12,X,Pargpv,Parmeter);
case 13, disp(' Example Himmelblau, p. 404 ')
   X     = [0; 0; 0; 0; 1];  % feasible
   X_OPT = [0.3000; 0.3335; 0.4000; 0.4285; 0.224];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 80;
   Lin_restr = 1; Tol = 1.0E-7;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp13,X,Pargpv,Parmeter);
case 14, disp(' Example Himmelblau, p. 406 ')
   X     = [78.62; 33.44; 31.07; 44.18; 35.22];
   X_OPT = [78; 33; 29.995; 45; 36.776];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 60;
   Lin_restr = 0; Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp14,X,Pargpv,Parmeter);
case 15, disp(' Example Himmelblau, p. 415 ')
   X     = [1; 1; 1; 1; 1; 1; 1; 1; 1]; % not feasible
   X_OPT = [0.9971; - 0.0758; 0.5530; 0.8331; 0.9981; ...
           -0.0623; 0.5642; 0.8256; 0.0000024];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 20;
   Lin_restr = 0; Tol = 1.0E-3;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter = [];
   [X,f,errorcode,PFAD] = gp_g(@bsp15,X,Pargpv,Parmeter);
case 16, disp(' Example Himmelblau, p. 395, SQP ')
   disp(' At first feasible point with ''sqp.m" then "gp.m" ')
   disp(' because ''Log'' in objective function ')
   disp(' Press any key ')
   pause
   X     = 0.1*ones(10,1);
   X_OPT = [0.0350; 0.1142; 0.8306; 0.0012; 0.4887;...
            0.0005; 0.0209; 0.0157; 0.0289; 0.0751];
   Tol = 1.0E-3; Maxit = 20; epsilon  = 4; beschl   = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter  = [];
   [X,f,errorcode] = sqp(@bsp16,X,Parsqp,Parmeter);
   save daten16 X
case 17, disp(' Example Himmelblau, p. 395 ')
   disp(' At first Ex. 16 for feasible point with ''sqp.m" ')
        (' because ''Log'' in objective function ')
   load daten16 X
   %  Start vector of no. 16 feasible
   %  Himmelblau, p. 395
   X_OPT = [0.0350; 0.1142; 0.8306; 0.0012; 0.4887;...
           0.0005; 0.0209; 0.0157; 0.0289; 0.0751];
   Accel = 2; Eps = 1.0E-5; Gamma = 0.5; Maxit = 60;
   Lin_restr = 0; Tol = 1.0E-5;
   Pargpv    = [Accel,Eps,Gamma,Maxit,Lin_restr,Tol];
   Parmeter  = [];
   [X,f,errorcode] = gp(@bsp16,X,Pargpv,Parmeter);
end;
% ----------------------------------------
disp(' ------------------- ')
if errorcode == 0
      disp(' Solution ');
      x_and_x_opt = [X, X_OPT], f
else
   disp(' Bad or no solution ');
   %Tol, Maxit
   x_and_x_opt = [X, X_OPT], f
   if nr == 16,  f_opt = -47.7, end
   switch errorcode
   case 1, disp(' Max. step number ')
   case 2, disp(' Max. step number in backtracking ')
   case 3, disp(' Start vector not feasible ')
   end
end;
