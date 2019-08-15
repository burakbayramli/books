function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 28.04.08
% Minimization of a function after Nelder and Mead,
% cf.  Himmelblau, S. 148 ff.
clc, clear, format short, format compact, nr = 100;
while ~ismember(nr,[1,2,3])
   nr   = input(' Beispiel Nr. (1/2/3) ');
end;
%nr = 3;
switch nr
case 1, disp(' Ex. Himmelblau, p. 155 ')
   PROBLEM = @bsp00; X_OPT = [5, 6];
   MAXITER = 12; TOL = 1.0E-5;
   RR = 0.07; % nur fuer Grafik
   Parmeter1 = [MAXITER,TOL,RR];
   X0 = [8;9];                 % Start
   X1 = [8, 10, 8; 9, 11, 11]; % Startsimplex
   bild01, pause(1)
   [X,PFAD] = simplex_g(PROBLEM,X1,Parmeter1);
   MAXFUN = 50; PHI = -1000; % Schranke AUS!
   Parmeter2 = [MAXITER,MAXFUN,TOL,PHI,RR];
 %  [X1,V1,F1] = fminsearchc('bsp001',X0,Parmeter2);
 %  fill(V1(1,:),V1(2,:),'g')
   plot(PFAD(1,:),PFAD(2,:),'k','linewidth',2)
case 2, disp(' Ex. Himmelblau, S. 153 ')
   PROBLEM = @bsp01; X_OPT = [1,1];
   MAXITER = 65; TOL = 1.0E-5; STEP = 0.5;
   RR = 0.01; % nur fuer Grafik
   Parmeter = [MAXITER,TOL,RR];
   X0 = [-1.2, 1];            % Start
   %X00 = X0';
   X1 = start(X0,STEP);      % Startsimplex
   X1 = X1';
   bild02, pause(1)
   [X,PFAD] = simplex_g(PROBLEM,X1,Parmeter);
   MAXFUN = 50; PHI = -1000; % Bound off!
   Parmeter2 = [MAXITER,MAXFUN,TOL,PHI,RR];
   X0 = X0';
   %[X1,V1,F1] = fminsearcha(PROBLEM,X0,Parmeter2);
   %fill(V1(1,:),V1(2,:),'g')
   %plot(PFAD(1,:),PFAD(2,:),'k','linewidth',2)
case 3, disp(' Ex. Himmelblau, S. 454, ')
   % does not work with simplex.m but with fminsearch.m
   PROBLEM = @bsp02; X_OPT = [0,0,0,0]
   MAXITER = 300; TOL = 1.0E-6; STEP = 0.5; RR = 0.01;
   STEP = 2;
   Parmeter = [MAXITER,TOL,RR];
   X0 = [3, -1, 0, 1];
   X0 = [0, 0, 0, 0];
   X1 = start(X0,STEP);
   X1 = X1';
   X  = simplex(PROBLEM,X1,Parmeter);
   F  = feval(PROBLEM,X);
   MAXITER = 30;
   MAXFUN = 90; PHI = -1000; % Schranke AUS!
   Parmeter2 = [MAXITER,MAXFUN,TOL,PHI,RR];
   X0 = X0';
   [X1,V1,F1] = fminsearcha(PROBLEM,X0,PHI,Parmeter2);
   F_X = [F,X']
   F1_X1 = [F1(1),X1']
end
