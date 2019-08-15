function demo2b
% Program for bifurcation of Poisson equation
% - Delta(u_0 + w) - f(mu_0 + mu,u_0 + w) = 0; UU'*w = 0;
% (- Delta - mu0*I)UU = 0; u_0 = Eps*UU*zeta 
% in unit square with zero boundary conditions for x = 0, x = 1
% there are three different eigenvalues considered
% with DIM Ker L = 1, 2, or 3
% 'bsp03a': f(mu,x) = mu*(x + cc*SIGN*x^3)
% 'bsp04a': f(mu,x) = mu*sin(X)
% 'bsp05a': f(mu,x) = mu*x + cc*SIGN*x^3, cc factor for demonstration 
% 'bsp06a': f(mu,x) = mu*(x + SIGN*x^2) % interesting
% 'bsp07a': f(mu,x) = mu*(x + SIGN*c(xi,eta)*x^3)
% 'bsp08a': f(mu,x) = mu*x + cc*SIGN*x^2, cc factor for demonstration 
% type 1: f(mu,x) = mu*x + g(mu,x); g(mu,x) = o(|x|)
% type 2: f(mu,x) = mu*g(x);

clear, clc, format short, format compact, 
Example = 100;
while ~ismember(Example,[3,4,5,6,7,8])
   Example = input(' Example no. (3/4/5/6/7/8)? ');
end;
%   Example = 3;
%%%%%%%%%%%%%%%%%%%%%%%%%
NU = 2; K = 1; % SELECT--SELECT: 1 <= NU <= 3, 1 <= K <= NU
%%%%%%%%%%%%%%%%%%%%%
% -- General parameters -------------------
n   = 24;    % Number of discretization points on X- resp. Y-axis
tol = 1E-6;  % Tolerance
global A UU VV
switch NU
case 1, [A,MU0,UU,VV] = laplace2(n);
case 2, [A,MU0,UU,VV] = laplace3(n);
case 3, [A,MU0,UU,VV] = laplace4(n);
end
switch Example
case 3, disp(' f(mu,x) = mu*(x + cc*SIGN*x^3) ')
   F = @bsp03b; G = @bsp03c;
   SIGN  = -1; type = 2;
   m = n+2; % h = 1/(n+1) step length, boundary points omitted
   % Choose operational parameters properly:
   switch NU
      case 1, Eps = 3;  Maxit = 12; cc = 20;
      case 2, Eps = 3;  Maxit = 20; cc = 15;
      case 3, Eps = 3;  Maxit = 30; cc = 3;
   end
case 4,  disp(' f(mu,x) = mu*sin(x) ')
   F = @bsp04b; G = @bsp04c;  
   SIGN = - 1; type = 2
   m = n+2; % h = 1/(n+1) step length, boundary points omitted
   % Choose operational parameters properly:
   switch NU
      case 1, Eps = 10; Maxit = 15; cc = 1;
      case 2, Eps = 10; Maxit = 15; cc = 1; %cc  not used
      case 3, Eps = 6;  Maxit = 20; cc = 1;
   end
case 5, disp(' f(mu,x) = mu*x + cc*SIGN*x^3 ')
   F = @bsp05b; G = @bsp05c;
   SIGN =  -1; type = 1
   m = n+2; % h = 1/(n+1) step length, boundary points omitted
   % Choose operational parameters properly:
   switch NU
      case 1, Eps = 10; Maxit = 10; cc = 50; 
      case 2, Eps = 10; Maxit = 15; cc = 50;
      case 3, Eps = 10; Maxit = 15; cc = 50;
   end
case 6, disp(' f(mu,x) = mu*(x + SIGN*x^2) ')
   F = @bsp06b; G = @bsp06c;
   SIGN = -1; type = 2
   m = n+2; % h = 1/(n+1) step length, boundary points omitted
   % Choose operational parameters properly:
   switch NU
      case 1, Eps = 5; Maxit = 12; cc = 1;
      case 2, Eps = 5; Maxit = 12; cc = 1; %cc not used
      case 3, Eps = 1; Maxit = 40; cc = 1;
   end
case 7,  disp(' f(mu,x) = mu*x + SIGN*c(xi,eta)*x^3 ')
   F = @bsp07b; G = @bsp07c; 
   SIGN = -1; type = 1;
   m = n+2; % h = 1/(n+1) step length, boundary points omitted
   % Choose operational parameters properly:
   switch NU
      case 1, Eps = 10; Maxit = 15; cc = 1;
      case 2, Eps = 10; Maxit = 10; cc = 1; % cc not used
      case 3, Eps =  7; Maxit = 10; cc = 1;
   end
case 8, disp(' f(mu,x) = mu*x + cc*SIGN*x^2 ')
   F = @bsp08b; G = @bsp08c;
   SIGN =  -1; type = 1;
   m = n+2; % h = 1/(n+1) step length, boundary points omitted
   % Choose operational parameters properly:
   switch NU
      case 1, Eps = 5; Maxit = 15; cc = 20;  
      case 2, Eps = 8; Maxit = 15; cc = 20; 
      case 3, Eps = 5; Maxit = 30; cc = 20;
   end  
end   
% -- Start values -----------------------
Parmeter = [0;NU;SIGN;n;K;tol;Maxit;Eps;m;cc;type];  
Parmeter(1) = MU0;
% -- Numerical Bifurcation ---------------
[Y,U0,MU_BIF,ecode] = bifurcation(F,G,Parmeter);
Parmeter(1) = MU_BIF;
RES = feval(F,Y,1,Parmeter);
RESIDUUMNORM = max(abs(RES))
% ----------------------------------------
MAXY = max(abs(Y)); MU0_MU_Xmax = [MU0,MU_BIF,MAXY]
switch Example
case 3, save daten3b Y U0 MU_BIF Parmeter,  bild02b(Example)
case 4, save daten4b Y U0 MU_BIF Parmeter,  bild02b(Example)
case 5, save daten5b Y U0 MU_BIF Parmeter,  bild02b(Example)
case 6, save daten6b Y U0 MU_BIF Parmeter,  bild02b(Example)
case 7, save daten7b Y U0 MU_BIF Parmeter,  bild02b(Example)
case 8, save daten8b Y U0 MU_BIF Parmeter,  bild02b(Example)
end
disp('Call bild02b(Example no.) for further figures ')
disp('or call demo3b(Example no.) for direct continuation ') 
grafik = 0;
if grafik == 1
   pause
   clf
   XA = linspace(0,1,n); YA = linspace(0,1,n+2);
   [UA,VA] = meshgrid(XA,YA);
   RES = reshape(RES,n+2,n);
   W1 = griddata(XA,YA,RES,UA,VA,'cubic');
   mesh(UA,VA,W1);
end

