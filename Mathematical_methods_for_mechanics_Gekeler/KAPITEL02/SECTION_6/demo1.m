function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Masterfile for multiple shooting method
% for periodic problems with estimated period
% Parameter-dependent problems, transformed to period one
% Beispiel 1: Nerve membran model,
%              vgl. Deuflhard, BIT 24 (1984), 456-466
% Beispiel 2: Heat flow problem
%              vgl. Deuflhard, BIT 24 (1984), 456-466
% Beispiel 3: Arenstorf orbit I
% ------------------------------------------------------
clc, format short, format compact
Example = 100;
while ~ismember(Example,[1,2,3])
   Example = input(' Example No. (1/2/3) ');
end
% ------------------------------------------------------
errorcode = 0;
switch Example
case 1, disp('Nerve membran model ')
   Maxit     = 12;           % Max. step number in Newton method
   tol       = 1E-3;        %  Breaking tolerance
   tol_i     = 5E-5;        %  Breaking tolerance for interior iteration
   Parmeter1 = [Maxit,tol,tol_i]; % Parameter for Newton method
   % -----------------------------------------------------
   n         = 2;            % Dimension of system
   m         = 11;           % Number of subintervals
   GG        = @bsp01;      % The current example
   % -----------------------------------------------------
   Periode   = 12;           % Estimated period
   ANF       = [3;1.5];      % Estimated start vector for IVP
   Parmeter3 = [Periode];    % Parameter for example
   % -----------------------------------------------------
   % Start trajectory for periodic problems -----
   % Solving IVP with estimated period and initial vector
   V0        = zeros(n*(m+1),1); % initial vector for derivatives
   Parmeter2 = [n;m;V0]; % Parameter for multiple shooting
   Flag      = 1;
   options   = odeset('reltol',1.0E-1,'abstol',1.0E-2);
    TT       = linspace(0,1,m+1);   % equidistant shooting points
   [TA,X0]   = ode23(GG,TT,ANF,options,Flag,Parmeter3);
   Parmeter3 = [];
case 2, disp(' Heat flow problem ')
   Maxit = 20; tol = 1E-3; tol_i = 5E-5;
   Parmeter1 = [Maxit,tol,tol_i]; % Parameter for Newton method
   % -----------------------------------------------------
   n = 3; m = 60; GG = @bsp02; 
   % -----------------------------------------------------
   Periode   = 0.95;         % Estimated period
   ANF       = [0;-28;140];  % Estimated initial vector for IVP
   sig = 16; b = 4; r = 153.083;
   Parmeter3 = [Periode,sig,b,r]; % Parameter for example
   % -- Start trajectory ------------------------------------
   V0        = zeros(n*(m+1),1); % initial vector for derivatives
   Parmeter2 = [n;m;V0];  % Parameter for multiple shooting
   Flag      = 1;
   options   = odeset('reltol',1.0E-1,'abstol',1.0E-2);
    TT       = linspace(0,1,m+1);   % equidistant shooting points
   [TA,X0]   = ode23(GG,TT,ANF,options,Flag,Parmeter3);
   Parmeter3 = Parmeter3(2:4);
case 3, disp(' Arenstorf-Orbit ')
   Maxit = 20; tol = 1E-3; tol_i = 1E-6;
   Parmeter1 = [Maxit,tol,tol_i]; % Parameter for Newton method
   % -----------------------------------------------------
   n = 4; m = 80; GG = @bsp03; 
   % -----------------------------------------------------
   Periode   = 6;              % Estimated period
   ANF       = [1.2;0;0;-1];   % Estimated start vector for IVP
 %  Periode   = 6.19231;        % exact Period
 %  ANF       = [1.2;0;0;-1.049357]; % exact initial vector
   mu       =  0.012128562765; % relative Moon mass
   Parmeter3 = [Periode,mu];   % Parameter for example
   %  -- Start trajectory for periodic problem -----
   V0        = zeros(n*(m+1),1); % initial vector for derivatives
   Parmeter2 = [n;m;V0];  % Parameter for multiple shooting
   Flag      = 1;
   options   = odeset('reltol',1.0E-3,'abstol',1.0E-3);
   TT        = linspace(0,1,m+1);   % equidistant shooting points
   [TA,X0]   = ode23(GG,TT,ANF,options,Flag,Parmeter3);
   Parmeter3 = Parmeter3(2);
end
% -- End of initialization ------------------------------
X0 = X0'; X0 = X0(:); X0 = [X0;Periode];
[X,errorcode] = newton_p(@mehrziel_p,X0,GG,Parmeter1,Parmeter2,Parmeter3);
LE = length(X); Periode = X(LE);
X = X(1:LE-1); X = reshape(X,n,m+1);
switch Example
case 1, save datena m X Periode
case 2, save datenb m X Periode
case 3, save datenc m X Periode
end
% -- Output --------------------------
switch Example
case 1, fig0221
case 2, fig0222
case 3, fig0223
end
disp(' ------------------- ')
if errorcode == 0,disp(' Solution ');
else, disp(' No or bad solution '); tol, Maxit
   if errorcode == 1, disp(' Max. step number attained '), end
   if errorcode == 2, disp(' Max. step number in backtracking attained ')
   end
end;
