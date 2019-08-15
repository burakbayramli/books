function demo3
% Eckart Gekeler, Universitaet Stuttgart, Release 19.04.08
% Flexiplex method after Himmelblau
% f(x) = Min!, g(x) >= 0, h(x) = 0
% Examples must have the form Y = examplename(X,flag,Parmeter);
% X state vector, Parmeter: additional parameters for problem
% flag = 1: Objective function
% flag = 2: Inequalities
% flag = 3: Equalities
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities
% flag = 6: Gradient of equalities
% optionally in 2-dimensional problems: 
% flag = 7: boundary of feasible domain 

clc, clear, format short, format compact, nr = 100;
while ~ismember(nr,[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])
   nr   = input(' Example No.(1/2/3/4/5/6/7/8/9/10/11/12/13/14/15) ');
end;
%nr = 4;
%% Grafik possible only for two-dimensional problems %%
GRAFIK = 1; % Grafik = (1/0)
%%%%%%%%%%%%%%%%%%%%%%%%%%%
switch nr
case 1, disp(' Bsp. Spellucci, S. 397 ')
   FNC1 = @bsp01; FNC2 = @bsp01b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 0.5;
   MAXITER1 = 50; PHI = 1.20; % Bound 
   ALFA = 1; BETA = 0.5; GAMA = 3; 
   X0 = [0; 0]; X_OPT = [-1; 0];
   if GRAFIK == 1, clf, bild07, circle(X0(1),X0(2),0.03,'k'); end
case 2, disp(' Bsp. Spellucci, S. 457 ')
   FNC1 = @bsp02; FNC2 = @bsp02b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 1;
   MAXITER1 = 50; PHI = 1.20;
   ALFA = 1; BETA = 0.5; GAMA = 3; 
   X0 = [-1; 1.5]; X_OPT = [0.546; 0.702];
   if GRAFIK == 1, bild08; circle(X0(1),X0(2),0.03,'k'); end
case 3, disp (' Bsp. Gekeler (1) ')
   FNC1 = @bsp03; FNC2 = @bsp03b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 1;
   MAXITER1 = 50; PHI = 1.20;  ALFA = 1; BETA = 0.5; GAMA = 3; 
   X0 = [-1;-2]; X_OPT = [3.45;0.19];
   if GRAFIK == 1, bild09; circle(X0(1),X0(2),0.03,'k'); end
case 4, disp(' Bsp. Gekeler (2)')
   FNC1 = @bsp04; FNC2 = @bsp04b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 1;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 3; 
   X0 = [-1;-2]; %X0 = [-1;0];  % o.k.
   X_OPT = [2.43;1.27];
   if GRAFIK == 1, bild10; circle(X0(1),X0(2),0.03,'k'); end
case 5, disp(' Bsp. Himmelblau, S. 393, nur Gleichungen ')
   FNC1 = @bsp05; FNC2 = @bsp05b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [2; 2]; X_OPT = [1.8; 1.4];
   GRAFIK = 0; 
case 6, disp(' Bsp. Himmelblau, S. 393, nur Ungleichungen ')
   FNC1 = @bsp06; FNC2 = @bsp06b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [2; 2]; X_OPT = [1.665; 0.554];
   GRAFIK = 0; 
case 7, disp(' Bsp. Himmelblau, S. 393 ')
   FNC1 = @bsp07; FNC2 = @bsp07b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [2; 2]; X_OPT = [0.823; 0.911];
   GRAFIK = 0; 
case 8, disp('Bsp. Himmelblau, S. 397 ')
   FNC1 = @bsp08; FNC2 = @bsp08b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [2; 2; 2]; X_OPT = [3.512; 0.217; 3.552];
   GRAFIK = 0; 
case 9, disp(' Bsp. Spellucci, S. 339 ')
   FNC1 = @bsp09; FNC2 = @bsp09b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0   = [0; 1/4; 1/2]; X_OPT = [0; 0; 2];
   GRAFIK = 0; 
case 10, disp(' Bsp. Spellucci, S. 370 (Rosen-Suzuki) ')
   FNC1 = @bsp10; FNC2 = @bsp10b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [0; 0; 0; 0]; X_OPT = [0; 1; 2; -1];
   GRAFIK = 0; 
case 11, disp(' Bsp. Spellucci, S. 368 (Gill-Murray-Wright) ')
   FNC1 = @bsp11; FNC2 = @bsp11b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [-1; -1]; X_OPT = [- 0.8165; - 1.155];
   GRAFIK = 0; 
case 12, disp(' Bsp. Himmelblau, S. 403 ')
   FNC1 = @bsp12; FNC2 = @bsp12b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [-3; -1; -3; -1]; X_OPT = [1; 1;  1; 1];
   GRAFIK = 0; 
case 13, disp(' Bsp. Himmelblau, S. 404 ')
   FNC1 = @bsp13; FNC2 = @bsp13b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [0; 0; 0; 0; 1];  % zulaessig
   X_OPT = [0.3000; 0.3335; 0.4000; 0.4285; 0.224];
   GRAFIK = 0; 
case 14, disp(' Bsp. Himmelblau, S. 406 ')
   FNC1 = @bsp14; FNC2 = @bsp14b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [78.62; 33.44; 31.07; 44.18; 35.22];
   X_OPT = [78; 33; 29.995; 45; 36.776];
   GRAFIK = 0; 
case 15, disp(' Bsp. Himmelblau, S. 415 ')
   X0    = [1; 1; 1; 1; 1; 1; 1; 1; 1];
   STEP = 20;
   FNC1 = @bsp15; FNC2 = @bsp15b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-7; %STEP = 3;
   MAXITER1 = 50; PHI = 6; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X_OPT = [0.9971; - 0.0758; 0.5530; 0.8331; 0.9981; ...
           -0.0623; 0.5642; 0.8256; 0.0000024];
   GRAFIK = 0; 
end   
% Parmeter1 for flexiplex, Parmeter2 for fminsearch
Parmeter1 = [ALFA,BETA,GAMA,STEP,GRAFIK,MAXITER1,TOL];
Parmeter2 = [MAXITER,MAXFUN,TOL];
Y = flexiplex(FNC1,FNC2,X0,PHI,Parmeter1,Parmeter2);
if isempty(X_OPT)
   APPROXIMATION = Y
else
   APPROX_XOPT = [Y,X_OPT]
end
FAPPR = feval(FNC1,Y,1)
if nr == 15,
   FOPT = -0.8660, KRIT = feval(FNC2,Y), 
end          
clear all
