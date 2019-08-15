function demo2
% Eckart Gekeler, Universitaet Stuttgart, Release 9.5.08
% Masterfile for nonlinear optimization problems
% Sequential quadratic optimization
% f(x) = Min!, g(x) >= 0, h(x) = 0
% INPUT  'name'  Name of the problem
%        X       Arbitrary start vector
%        Tol     Tolerance
%        Maxit   Max. step number
% OUTPUT X       Solution if success
%        f       Value of objective function
% Examples must have the form Y = examplename(X,flag,Parmeter);
% X state vector, Parmeter: additional parameters for problem
% flag = 1: Objective function f
% flag = 2: Inequalities       g
% flag = 3: Equalities         h
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities
% flag = 6: Gradient of equalities
% optionally in 2-dimensional problems: 
% flag = 7: boundary of feasible domain 
%
clc, format short, format compact
errorcode = 0;
nr = 100; KK = [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15];
while ~ismember(nr,KK)
   nr = input(' Beispiel Nr. (1/2/3/4/5/6/7/8/9/10/11/12/13/14/15) ');
end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
switch nr
case 1, disp(' Example Spellucci, p. 397 ')
   for I = 1:4
      if I == 1, X = [-1; 2];  end
      if I == 2, X = [-1; -2]; end
      if I == 3, X = [1; -2];  end
      if I == 4  X = [1; 2];   end
      X_OPT    = [-1; 0];
      Tol = 1.0E-5; Maxit = 20; epsilon  = 5; beschl = 1;
      BeginMaratos = Maxit + 1;
      Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
      Parmeter = [];
      [X,f,errorcode,PFAD] = sqp_g(@bsp01,X,Parsqp,Parmeter);
 %     [X,f,errorcode,PFAD] = sqp_g(@bsp01a,X,Parsqp,Parmeter);
      if I == 1, save datens1a PFAD, end
      if I == 2, save datens1b PFAD, end
      if I == 3, save datens1c PFAD, end
      if I == 4, save datens1d PFAD, end
      pause(1)    
   end
   bild01(2)
case 2, disp(' Example Spellucci, p. 457, convex problem ')
   X = [-1; 1.5]; X_OPT = [0.546096792; 0.701778292];
   Tol = 1.0E-5; Maxit = 20; epsilon = 3; beschl = 2;
   BeginMaratos = Maxit + 1; % no maratos
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
  % [X,f,errorcode,PFAD] = sqp_g(@bsp02,X,Parsqp,Parmeter);
   [X,f,errorcode,PFAD] = sqp_g(@bsp02a,X,Parsqp,Parmeter);
   save datens2 PFAD
   bild02(2)
case 3, disp (' Example 1 Gekeler ')
   for I = 1:2
   if I == 1, X = [-1; -2]; end
   if I == 2, X = [-1; 2];  end
   X_OPT = [3.45455547048392;  0.186994753742436];   
   Tol = 1.0E-8; Maxit = 20; epsilon  = 1; beschl = 1;
   BeginMaratos = Maxit +1; % no maratos
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
   [X,f,errorcode,PFAD] = sqp_g(@bsp03,X,Parsqp,Parmeter);
  % [X,f,errorcode,PFAD] = sqp_g(@bsp03a,X,Parsqp,Parmeter);
   if I == 1, save datens3a PFAD, end
   if I == 2, save datens3b PFAD, end
   end
   bild03(2)
case 4, disp(' Example 2 Gekeler, convex problem ')
   X_OPT = [2.43199541642619; 1.26865405738502 ];
   Tol = 1.0E-7; Maxit = 30; epsilon  = 1; beschl = 2;  %2
   % For Figure with worse results
%    Tol = 1.0E-7; Maxit = 40; epsilon  = 4; beschl = 2;  %2
   X1 = []; X2 = []; X3 = [];
   for I=1:3  
   if I == 1, X = [-1; -2]; end
   if I == 2, X = [-1; 2]; end  
   if I == 3, X = [4; -2]; end
   BeginMaratos = Maxit +1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = []; 
   [X,f,errorcode,PFAD] = sqp_g(@bsp04,X,Parsqp,Parmeter);
  % [X,f,errorcode,PFAD] = sqp_g(@bsp04a,X,Parsqp,Parmeter);
   if I == 1, save datens4a PFAD, X1 = X; end 
   if I == 2, save datens4b PFAD, X2 = X; end
   if I == 3, save datens4c PFAD, X3 = X; end
   end
   X1_X2_X_3_X_OPT = [X1,X2,X3,X_OPT]
   bild04(2)
case 5, disp(' Example Himmelblau, p. 393, only equalities ')
   X     = [2; 2]; % not feasible
   X_OPT = [1.8; 1.4];
   Tol = 1.0E-5; Maxit = 20; epsilon  = 4; beschl   = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
 %  [X,f,errorcode] = sqp_h(@bsp05,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp_h(@bsp05a,X,Parsqp,Parmeter);
case 6, disp(' Example Himmelblau, p. 393, only inequalities ')
   X = [2; 2]; X_OPT = [1.665; 0.554];
   Tol = 1.0E-5; Maxit = 20; epsilon  = 1; beschl   = 2;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
 %  [X,f,errorcode,PFAD] = sqp_g(@bsp06,X,Parsqp,Parmeter);
   [X,f,errorcode,PFAD] = sqp_g(@bsp06a,X,Parsqp,Parmeter);
case 7, disp(' Example Himmelblau, p. 393 ')
   X     = [2; 2];
   X_OPT = [0.823; 0.911];
   Tol = 1.0E-5; Maxit = 20; epsilon  = 4; beschl   = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
 %  [X,f,errorcode] = sqp(@bsp07,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp(@bsp07a,X,Parsqp,Parmeter);
case 8, disp('Example Himmelblau, p. 397 ')
   X     = [2; 2; 2];
%  X     = [10; 10; 10];
%  X     = [3.5668; 0.2109; 3.5019];
   X_OPT = [3.512; 0.217; 3.552];
   Tol = 1.0E-5; Maxit = 20; epsilon = 4;  beschl = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
  % [X,f,errorcode] = sqp(@bsp08,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp(@bsp08a,X,Parsqp,Parmeter);
case 9, disp(' Example Spellucci, p. 339 ')
   X   = [0; 1/4; 1/2]; X_OPT = [0; 0; 2];
   Tol = 1.0E-5; Maxit = 20; epsilon  = 4; beschl = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
 %  [X,f,errorcode,PFAD] = sqp_g(@bsp09,X,Parsqp,Parmeter);
   [X,f,errorcode,PFAD] = sqp_g(@bsp09a,X,Parsqp,Parmeter);
case 10, disp(' Example Spellucci, p. 370 (Rosen-Suzuki) ')
   X   = [0; 0; 0; 0]; X_OPT = [0; 1; 2; -1];
   Tol = 1.0E-5; Maxit = 20; epsilon  = 4;  beschl = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
 %  [X,f,errorcode,PFAD] = sqp_g(@bsp10,X,Parsqp,Parmeter);
   [X,f,errorcode,PFAD] = sqp_g(@bsp10a,X,Parsqp,Parmeter);
case 11, disp(' Example Spellucci, p. 368 (Gill-Murray-Wright) ')
%   X    = [-0.1; -0.1];
   X     = [-1; -1];
%   X    =[-10;-10];
   X_OPT = [- 0.8165; - 1.155];
   Tol = 1.0E-5; Maxit = 20; epsilon = 4; beschl = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
  [X,f,errorcode] = sqp_h(@bsp11,X,Parsqp,Parmeter);
%   [X,f,errorcode] = sqp_h(@bsp11a,X,Parsqp,Parmeter);
   errorcode
case 12, disp(' Example Himmelblau, p. 403 ')
   X     = [-3; -1; -3; -1];  % feasible
   X_OPT = [1; 1;  1; 1];
   Tol = 1.0E-10; Maxit = 110; epsilon = 4; beschl = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
%   [X,f,errorcode,PFAD] = sqp_g(@bsp12,X,Parsqp,Parmeter);
   [X,f,errorcode,PFAD] = sqp_g(@bsp12a,X,Parsqp,Parmeter);
case 13, disp(' Example Himmelblau, p. 404 ')
   X     = [0; 0; 0; 0; 1];  % feasible
   X_OPT = [0.3000; 0.3335; 0.4000; 0.4285; 0.224];
   Tol = 1.0E-9; Maxit = 40; epsilon = 4; beschl = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
   %[X,f,errorcode,PFAD] = sqp_g(@bsp13,X,Parsqp,Parmeter);
   [X,f,errorcode,PFAD] = sqp_g(@bsp13a,X,Parsqp,Parmeter);
case 14, disp(' Example Himmelblau, p. 406 ')
   X     = [78.62; 33.44; 31.07; 44.18; 35.22];
   X_OPT = [78; 33; 29.995; 45; 36.776];
   Tol = 1.0E-5; Maxit = 20; epsilon = 4; beschl = 1;
   BeginMaratos = Maxit + 1;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
 %  [X,f,errorcode,PFAD] = sqp_g(@bsp14,X,Parsqp,Parmeter);
   [X,f,errorcode,PFAD] = sqp_g(@bsp14a,X,Parsqp,Parmeter);
case 15, disp(' Example Himmelblau, p. 415 ')
   X     = [1; 1; 1; 1; 1; 1; 1; 1; 1];
   X_OPT = [0.9971; - 0.0758; 0.5530; 0.8331; 0.9981; ...
           -0.0623; 0.5642; 0.8256; 0.0000024];
   Tol = 1.0E-9; Maxit = 40; epsilon  = 1; beschl = 1;
   BeginMaratos = Maxit-30;
   Parsqp   = [Tol,Maxit,epsilon,beschl,BeginMaratos];
   Parmeter = [];
   [X,f,errorcode,PFAD] = sqp_g(@bsp15,X,Parsqp,Parmeter);
 %  [X,f,errorcode,PFAD] = sqp_g(@bsp15a,X,Parsqp,Parmeter);
    % Something seems wrong because f_opt = -0.86589068088 
    % with x_opt after Himmelblau but f_opt = -0.8660
    % is the value given there
end;
% ----------------------------------------
disp(' --------------- ')
flag = 1;
if flag == 1
if errorcode == 0
   disp(' Solution ');
   if isempty(X_OPT), X
   else, X_and_X_OPT = [X, X_OPT]
   end
   F_OPT = f
else
   disp(' No or bad solution ');
   if isempty(X_OPT), X
   else, X_and_X_OPT = [X, X_OPT]
   end
   F_OPT = f
   switch errorcode
   case 1, disp(' Max. step number ')
   case 2, disp(' Max. step number in backtracking ')
   case 3, disp(' Start vector not feasible ')
   end
end;
if nr == 15, disp('f(x_opt) = - 0.8660 '); end
end
%PFAD
clear all  