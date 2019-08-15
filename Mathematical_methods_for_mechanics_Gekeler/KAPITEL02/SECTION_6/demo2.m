function demo2
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Masterfile for multiple shooting method
% for periodic problems with estimated period
% Parameter-dependent Problem, transformed to period 1
% Exmple 4 : Duffing equation

clc, format short, format compact
% ------------------------------------------------------
errorcode = 0; 
disp(' Duffing equation ')
Maxit = 12;   % Max. step number in Newton method
tol   = 1E-3; % Breaking tolerance
tol_i = 5E-5; % Breaking tolerance for interior iteration
Parmeter1 = [Maxit,tol,tol_i]; % Parameter for Newton method
% -----------------------------------------------------
n = 2;        % Dimension of system
m = 120;      % number of subintervals
G = @bsp04;   % current example
% -----------------------------------------------------
Periode = 12;        % Estimated period
ANF     = [2.72;0];  % Estimated initial vector
a       = 0.1;       % Parameter of Example 4
b       = 5;         % Parameter of Example 4
Parmeter3 = [Periode,a,b]; % Parameter for example
% -----------------------------------------------------
%  Start trajectory for periodic problem
V0        = zeros(n*(m+1),1); % initial vector of derivatives
Parmeter2 = [n;m;V0];         % Parameter for multiple shooting
Flag      = 1;
options = odeset('reltol',1.0E-6,'abstol',1.0E-3);
TT      = linspace(0,1,m+1);  % equidistant shooting points
[TA,X0] = ode23(G,TT,ANF,options,Flag,Parmeter3);
BB = b; PP = Periode;         % Monitoring
XANF = X0;
% -- End of initialization ------------------------------
X0 = X0'; X0 = X0(:); X = [X0;Periode];
for I = 1:10 % 10
    b = b + 0.2; Parmeter3 = [a,b]; X0 = X;
   [X,errorcode] = newton_p('mehrziel_p',X0,G,Parmeter1,Parmeter2,Parmeter3);
   disp(' ------------------- ')
   switch errorcode  
      case 0, disp(' Solution ');
      case 1, disp(' Max. step number ')
      case 2, disp(' Max. step number in backtracking ')
   end
   LE = length(X);  Periode = X(LE) 
   BB = [BB,b]; PP = [PP,Periode];
   XA = X(1:LE-1); 
   XA = reshape(XA,n,m+1);
   save datend m XA Periode XANF
   fig0224a
   pause(1)
end
