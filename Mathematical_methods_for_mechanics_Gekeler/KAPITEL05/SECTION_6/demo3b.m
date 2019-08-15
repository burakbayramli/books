function demo3b
% Continuation for MU, after start by DEMO2B.M
% - Delta(u0 + w) - f(mu0 + mu,u0 + w) = 0; UU'*w = 0;
% Maxit_Cont : step number in continuation
% Maxit      : Max. step number in Newton iteration
% D_MU       : step length for MU in iteration;

clear, clc, format short, format compact
errorcode = 0;
disp(' For EACH run call first DEMO2B.M for the selected example! ')
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
   load daten3b Y U0 MU_BIF Parmeter, F = @bsp03b;
   Maxit_Cont = 5; Maxit = 10; D_MU = 20;
   NU = Parmeter(2); if NU == 2, Y = 2*Y; end
case 4
   load daten4b Y U0 MU_BIF Parmeter, F = @bsp04b;
   Maxit_Cont = 5;  Maxit = 7; D_MU = 0.5;
   NU = Parmeter(2); if NU == 2, Y = 2*Y; end
   switch NU
      case 1, D_MU = 0.5;
      case 2, D_MU = 1;
      case 3, D_MU = 1;
   end     
case 5
   load daten5b Y U0 MU_BIF Parmeter, F = @bsp05b;
   Maxit_Cont = 5; Maxit = 7; D_MU = 20;
case 6
   load daten6b Y U0 MU_BIF Parmeter, F = @bsp06b;
   Maxit_Cont = 3; Maxit = 5; 
   NU = Parmeter(2); if NU == 2, Y = 4*Y; end
   switch NU
      case 1, D_MU = 1;
      case 2, D_MU = 2;
      case 3, D_MU = 2; Maxit_Cont = 10;
   end     
case 7
   load daten7b Y U0 MU_BIF Parmeter, F = @bsp07b;
   Maxit_Cont = 10; Maxit = 7; 
   NU = Parmeter(2);
   switch NU
      case 1, D_MU = 1; 
      case 2, D_MU = 1;
      case 3, D_MU = 1;
   end     
case 8
   load daten8b Y U0 MU_BIF Parmeter, F = @bsp08b;
   Maxit_Cont = 3; Maxit = 7; D_MU = 20;
   NU = Parmeter(2);
   switch NU
      case 1, D_MU = 1;
      case 2, D_MU = 1;
      case 3, D_MU = 1; %fails
   end     
end
Parmeter(1) = MU_BIF;
for I = 1:Maxit_Cont
   X0 = Y;
   Parmeter(1) = Parmeter(1) + D_MU; % Change of MU
   [Y,ecode] = newton(F,X0,tol,Maxit,Parmeter);
   MU_CONT = Parmeter(1);
   I_MU0_MU = [I,MU_BIF,MU_CONT]
end
RES   = feval(F,Y,1,Parmeter);
RESIDUUMNORM = max(abs(RES))

switch example
case 3, save daten3bcont Y U0 MU_BIF Parmeter,  bild03b(example)
case 4, save daten4bcont Y U0 MU_BIF Parmeter,  bild03b(example)
case 5, save daten5bcont Y U0 MU_BIF Parmeter,  bild03b(example)
case 6, save daten6bcont Y U0 MU_BIF Parmeter,  bild03b(example)
case 7, save daten7bcont Y U0 MU_BIF Parmeter,  bild03b(example)
case 8, save daten8bcont Y U0 MU_BIF Parmeter,  bild03b(example)
end
disp('Call bild03b(Example no.) for further figures ')

%switch ecode
%case 0, disp('Solution')
%case 1, disp('Bad initial value')
%case 2, disp('Bad condition for initial value')
%case 3, disp('failure at minimal stepsize ')
%case 4, disp(' maximal number of jacobian eval. exceeded ')
%end
