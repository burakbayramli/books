function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Masterfile for multiple shooting method
% Example 1: Stoer-Bulirsch, Par. 7.3, Bsp. 01
% Examples with Newton + Box s. KAPITEL04/CONTROL02
% and KAPTEL03/NEWTNTST
% ------------------------------------------------------
clc, format short, format compact
%while ~ismember(Mehrziel,[0,1])
%   Mehrziel  = input(' Box- oder multiple shooting ? (0/1)');
%end;
Mehrziel = 1;
bsp = 1;
errorcode = 0;
if bsp == 1
   Maxit  = 10;   % Max. step number in Newton method
   tol    = 1E-3; % Breaking tolerance 
   tol_i  = 1E-5; % Breaking tolerance for interior iteration
   % -----------------------------------------------
   n      = 2;            % Dimension of system
   m      = 20;           % Number of sub-intervals
   T_END  = 1;            % Length of interval of def.
   GG     = @bsp01;      % The current example
   Parmtr1 = [Maxit,tol,tol_i,n]; % Parameter for Newton
   Parmtr3 = [T_END];      % Parameter for example
   % -----------------------------------------------
   %  Start trajectory : straight connection between terminal points --
   TT = linspace(0,1,m+1);
   X0 = [TT; ones(1,m+1)];
   if Mehrziel == 1
      % -- Adaption of shooting points for bsp01.m -----------
      Maxwert    = 10;          % bound for adaption
      [MZP,X0,m] = adapt01(X0,GG,TT,Maxwert,Parmtr3);
      Parmtr2 = [n,m,MZP]; 
   end
   if Mehrziel == 0, Parmtr2 = [n,m]; MZP = TT;
   end
   % --------------------------------------------------
   X0 = X0(:);
   if Mehrziel == 0
      [X,errorcode] = newton(@box,X0,GG,Parmtr1,Parmtr2,Parmtr3);
   end
   if Mehrziel == 1
      [X,errorcode] = newton(@mehrziel,X0,GG,Parmtr1,Parmtr2,Parmtr3);
   end
   X = reshape(X,n,m+1);
   save daten MZP X
   fig0219
end
disp(' ------------------- ')
if errorcode == 0, disp(' Solution ');
else, disp(' No or bad solution '); tol, Maxit
   if errorcode == 1, disp(' Max. step number attained '), end
   if errorcode == 2, disp(' Max. step number in backtracking '), end
end;
clear
