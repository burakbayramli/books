function demo2
% Eckart Gekeler, Universitaet Stuttgart, Release 19.04.08
% Flexiplex method after Himmelblau
% f(x) = Min!, g(x) >= 0, h(x) = 0
% Examples must have the form Y = examplename(X,flag,Parmeter);
% X state vector, Parmeter: additional parameters for problem
% For some improvements see Himmelblau
% flag = 1: Objective function f
% flag = 2: Inequalities       g
% flag = 3: Equalities         h 
% flag = 4: Gradient of objective function
% flag = 5: Gradient of inequalities
% flag = 6: Gradient of equalities
% optionally in 2-dimensional problems: 
% flag = 7: boundary of feasible domain 

clc, clear, format short, format compact, nr = 100;
while ~ismember(nr,[1,2,3,4,5])
   nr   = input(' Example No.(1/2/3/4/5) ');
end;
%nr = 4;
%% Grafik possible only for two-dimensional problems %%
GRAFIK = 1; % Grafik = (1/0)
%%%%%%%%%%%%%%%%%%%%%%%%%%%
switch nr
case 1, disp(' Ex. Himmelblau, S. 359/360 ')
   X0 = [1;1]; % Start vector 
   FNC1 = @bsp03; FNC2 = @bsp03b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 0.5;
   MAXITER1 = 50; PHI = 1.20; % Bound 
   ALFA = 1; BETA = 0.5; GAMA = 3; XOPT = [1.0014;4.8987];
   if GRAFIK == 1, clf, bild03, circle(X0(1),X0(2),0.03,'k'); end
case 2, disp(' Ex. Spellucci, p. 397')
   FNC1 = @bsp04; FNC2 = @bsp04b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-5; STEP = 1;
   MAXITER1 = 50; PHI = 1.20;
   ALFA = 1; BETA = 0.5; GAMA = 3; 
   X0 = [2;1]; XOPT = [-1;0];
   if GRAFIK == 1, bild04; circle(X0(1),X0(2),0.03,'k'); end
case 3, disp(' Ex. Spellucci, p. 457 ')
   FNC1 = @bsp05; FNC2 = @bsp05b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-9; STEP = 1;
   MAXITER1 = 50; PHI = 1.20;  ALFA = 1; BETA = 0.5; GAMA = 3; 
   X0 = [-1;1.5]; % Start vector    %X0 = [-1;0];
   XOPT = [0.546; 0.702];
   if GRAFIK == 1, bild05; circle(X0(1),X0(2),0.03,'k'); end
case 4, disp(' Ex. 1 Gekeler ')
   FNC1 = @bsp06; FNC2 = @bsp06b;
   MAXITER = 100; MAXFUN = 200; TOL = 1.0E-7; STEP = 1;
   STEP = 3;
   MAXITER1 = 50; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 3; 
   X0 = [-1;-2]; %X0 = [-1;0];  % o.k.
   XOPT = [3.45;0.19];
   if GRAFIK == 1, bild06; circle(X0(1),X0(2),0.03,'k'); end
case 5, disp(' Ex. 2 Gekeler ')
   FNC1 = @bsp07; FNC2 = @bsp07b;
   MAXITER = 200; MAXFUN = 300; TOL = 1.0E-9; STEP = 3;
   MAXITER1 = 100; PHI = 1.20; ALFA = 1; BETA = 0.5; GAMA = 2; 
   X0 = [-1;-2]; XOPT = [2.43;1.27];
   % also X0 = [-1;0];
   if GRAFIK == 1, bild07; circle(X0(1),X0(2),0.03,'k'); end
end   
% Parmeter1 for flexiplex, Parmeter2 for fminsearch
Parmeter1 = [ALFA,BETA,GAMA,STEP,GRAFIK,MAXITER1,TOL];
Parmeter2 = [MAXITER,MAXFUN,TOL];
Y = flexiplex(FNC1,FNC2,X0,PHI,Parmeter1,Parmeter2);

if isempty(XOPT),  APPROXIMATION = Y
else,              APPROX_XOPT = [Y,XOPT]
end
clear all
