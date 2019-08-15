function demo3a
% Continuation for MU, after start by DEMO2A.M
% - Delta(u0 + w) - f(mu0 + mu,u0 + w) = 0; UU'*w = 0;
% Maxit_Cont : step number in continuation
% Maxit      : Max. step number in Newton iteration
% D_MU       : step length for MU in iteration;

clear, clc, format short, format compact
errorcode = 0;
disp(' For EACH run call first DEMO2A.M for the selected example! ')
disp(' Adapt parameters properly for selected example, NU and K ')
example = 100;
while ~ismember(example,[3,4,5,6,7,8])
   example = input(' example no. (3/4/5/6/7/8)? ');
end;
% -- General Parameters
tol = 1E-6;  % Tolerance
% --------------------------------------------------
switch example
case 3
   load daten3a Y U0 MU_BIF Parmeter, F = @bsp03a;
   Maxit_Cont = 5; Maxit = 10; D_MU = 20;
   NU = Parmeter(2); if NU == 2, Y = 2*Y; end
case 4
   load daten4a Y U0 MU_BIF Parmeter, F = @bsp04a;
   Maxit_Cont = 5;  Maxit = 7; D_MU = 20;
   NU = Parmeter(2); if NU == 2, Y = 2*Y; end
case 5
   load daten5a Y U0 MU_BIF Parmeter, F = @bsp05a;
   Maxit_Cont = 5; Maxit = 7; D_MU = 20;
case 6
   load daten6a Y U0 MU_BIF Parmeter, F = @bsp06a;
   Maxit_Cont = 5; Maxit = 7; D_MU = 20;
   NU = Parmeter(2); if NU == 2, Y = 4*Y; end
case 7
   load daten7a Y U0 MU_BIF Parmeter, F = @bsp07a;
   Maxit_Cont = 5; Maxit = 7; D_MU = 20;
case 8
   load daten8a Y U0 MU_BIF Parmeter, F = @bsp08a;
   Maxit_Cont = 5; Maxit = 7; D_MU = 20;
end
% -- MU_START = Parmeter(1) ------
for I = 1:Maxit_Cont
   X0 = Y;
   Parmeter(1) = Parmeter(1) + D_MU; % Change of MU
   [Y,ecode] = newton(F,X0,tol,Maxit,Parmeter);
   MU_CONT = Parmeter(1);
   I_MU0_MU = [I,MU_BIF,MU_CONT]
end
switch example
case 3, save daten3a Y U0 MU_BIF Parmeter,  bild02a(example)
case 4, save daten4a Y U0 Parmeter,  bild02a(example)
case 5, save daten5a Y U0 Parmeter,  bild02a(example)
case 6, save daten6a Y U0 Parmeter,  bild02a(example)
case 7, save daten7a Y U0 Parmeter,  bild02a(example)
case 8, save daten8a Y U0 Parmeter,  bild02a(example)
end
%bild03a
RES   = feval(F,Y,1,Parmeter);
RESIDUUMNORM = norm(RES)

%switch ecode
%case 0, disp('Solution')
%case 1, disp('Bad initial value')
%case 2, disp('Bad condition for initial value')
%case 3, disp('failure at minimal stepsize ')
%case 4, disp(' maximal number of jacobian eval. exceeded ')
%end
