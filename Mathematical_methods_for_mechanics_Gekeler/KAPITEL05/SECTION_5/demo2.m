function demo2
% Program for continuation after Hopf bifurcation
% omega*u' + A*u + mu*B*u + f(mu,u) = 0 in R^p, u(0) = u(2*pi)
% ATTENTION: Call first DEMO1.M !!
% Continuation only for backward differences !!!!
% FUNCTIONS: bdf.m, cg_lq.m
% USER DEFINED FUNCTIONS: bsp0x.m
%
clear, clc, format short, format compact
errorcode = 0;
nr = 100; KK = [1,2,3,4,5,7];
while ~ismember(nr,KK)
   nr   = input(' Example Nr. (1/2/3/4/5/7)? ');
end;
disp(' Call first DEMO1 with all options!'), pause(1)
%-------------------------------------------------
switch nr
case 1, disp(' Van der Pol equation ')
   load datenB1 Y omga mu Parmeter, F = 'bsp01';
   tol   = 1E-8; % Tolerance
   maxit = 20;   % Max. step number in Newton method
   MU  = [mu,0.1,0.3,0.6,0.9,1.2,1.5,1.8,2];
case 2, disp(' Feedback Inhibition Modell ')
   load datenB2 Y omga mu Parmeter
   F = 'bsp02'; tol = 1E-8; maxit = 20; MU = [mu,1,1.5,4,8];
case 3, disp(' Small Brusselator ')
   load datenB3 Y omga mu Parmeter
   F = 'bsp03'; tol = 1E-5; maxit = 20; MU = [mu,0.3,0.6,0.9,1.2,1.5];
case 4, disp(' Full Brusselator ')
   load datenB4 Y omga mu Parmeter
   F = 'bsp04'; tol = 1E-8; maxit = 20; MU = [mu,0.1,0.3,0.5,0.7,0.9,1.1];
case 5, disp(' Lorentz equation ')
   load datenB5 Y omga mu Parmeter
   F = 'bsp05'; tol = 1E-5; maxit = 20; MU = [mu,-0.1,-0.3,-0.5];
case 7, disp(' Test example ')
   load datenB7 Y omga mu Parmeter
   F = 'bsp07'; tol = 1E-5; maxit = 20; MU = [mu,0.2,0.4,0.6];
end
Parcont = [tol, maxit, omga, mu]; YY = Y; OMGA = omga;
% -- Continuation ----------------- 
for I = 2:length(MU)
   mu = MU(I); Parcont = [tol, maxit, omga, mu];
   [U,omga,errorcode] = hopf_contin(F,Y,Parcont,Parmeter);
   Y = U; YY = [YY;U]; OMGA = [OMGA; omga];
   iter_omga = [I,omga]
end
switch nr
case 1, save datenC1 YY OMGA MU, fig0510
case 2, save datenC2 YY OMGA MU, fig0511
case 3, save datenC3 YY OMGA MU, fig0512
case 4, save datenC4 YY OMGA MU Parmeter, fig0513
case 5, save datenC5 YY OMGA MU Parmeter, fig0514
case 7, save datenC7 YY OMGA MU Parmeter, bild01
end
disp(' ------------------- ')
if errorcode == 0, disp(' Solution ');
else, disp(' Bad or no solution ');
   if errorcode == 2, disp(' max. step number in iteration '), end
end;
