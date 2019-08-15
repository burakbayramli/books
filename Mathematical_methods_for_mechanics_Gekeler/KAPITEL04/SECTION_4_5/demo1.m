function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 24.8.06
% Control Problems with SQP.M
% Regularity condition must be fulfilled!
% uses sqp_h.m, sqp.m, maratos.m
clear, clc, format compact, format short
nr = 100; KK = [1,2,3,4,5,6,7,8,9];
while ~ismember(nr,KK)
   nr = input(' Beispiel Nr. (1/2/3/4/5/6/7/8/9) ');
end;
errorcode = 0;
%-------------------------------------------------
switch nr
case 1, disp(' Thrust Problem Bryson-Ho, S. 59 ')
   Maxit = 15;      % Max. step number in sqp.m
   n     = 10;      % Number of equidist. time intervals
   a     = 1;       % Constant of problem
   T_END = 2;       % Length of time interval
   Tol   = 1.0E-2;  % Estimated tolerance
   epsilon  = 4; 
   acceleration = 1; BeginMaratos = 20;
   tol_dlqp = 1E-6; Eps_dlqp = 0;
   Pardlqp  = [tol_dlqp,Eps_dlqp];
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   XN = zeros(4*(n+1),1);  % Nominal state
   UN = zeros(n+1,1);      % Nominal control

   for Height = 1:4
      X  =  [XN;UN];
      switch Height
         case 1, H = 0.8; case 2, H = 0.6;
         case 3, H = 0.4; case 4, H = 0.2;
      end
       Parmeter = [n, a, T_END, H];
    %  [X,f,errorcode] = sqp_h(@bsp01,X,Parsqp,Pardlqp,Parmeter);
      [X,f,errorcode] = sqp_h(@bsp01a,X,Parsqp,Pardlqp,Parmeter);
      switch Height
         case 1, save daten01a X Parmeter
         case 2, save daten01b X Parmeter
         case 3, save daten01c X Parmeter
         case 4, save daten01d X Parmeter
      end
      disp(' ---------------- ')
   end
   fig0405
case 2, disp(' Orbit Problem Bryson-Ho, S. 66 ')
   Maxit = 100;     % max. step number in sqp.m
   n     = 40;      % number of equidist. time intervals
   a     = 0.14;    % Problem constant
   T_END = 2;       % Length of time interval
   Tol   = 1.0E-11; % Stop tolerance
   epsilon = 4;
    acceleration = 1; BeginMaratos = 200;
   tol_dlqp = 1E-6; Eps_dlqp = 0;
   Pardlqp  = [tol_dlqp,Eps_dlqp];
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n, a, T_END];
   XN1 = ones(n+1,1);                % Nominal state 1
   XN2 = zeros(n+1,1);               % Nominal state 2
   XN3 = 1 + linspace(0,T_END,n+1)'; % Nominal state 3
   UN  = zeros(n+1,1);               % Nominal control
   X   = [XN1;XN2;XN3;UN];
   %[X,f,errorcode] = sqp_h(@bsp02,X,Parsqp,Pardlqp,Parmeter);
   [X,f,errorcode] = sqp_h(@bsp02a,X,Parsqp,Pardlqp,Parmeter);
   save daten02 X Parmeter
   fig0406
case 3, disp(' Zermelos Problem, cf. Bryson-Ho, p. 67 ')
   % with transformation, T_END is control parameter
   Maxit = 20;          % max. step number in sqp.m
   n     = 20;          % number of equidist. time intervals
   S     = 1;           % Problem constant
   T_END = 1;
   A   = [3.61;-1.8]; % initial point
   A   = [4;-2];      % initial point
   B   = [0;0];       % terminal point
   Tol = 1.0E-5;      % stop tolerance
   fac = [1/16,1/8,1/4,1/2,1];  % Factor of S for FLUSS.M
   epsilon = 4;
   acceleration  = 1; BeginMaratos = 30;
   tol_dlqp = 1E-6; Eps_dlqp = 0;
   Pardlqp  = [tol_dlqp,Eps_dlqp];
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n;S;A;B;T_END];
   a = winkel(A,B,S); % angle of ship's axis
   % for current v = [-S/2; 0] and direct connection of A and B
   % trivial initial value: straight line between A and B
   % with angle a of the ship's axis
   XN1 = linspace(A(1),B(1),n+1)';  % Nominal state 1
   XN2 = linspace(A(2),B(2),n+1)';  % Nominal state 2
   UN  = a*ones(n+1,1);             % Nominal control
   TN  = 5;                         % Nominal time
   X   = [XN1;XN2;UN;TN];
   for I = 1:length(fac)
      FAKTOR = fac(I); Parmeter(8) = FAKTOR;
     % [X,f,errorcode] = sqp_h(@bsp03,X,Parsqp,Pardlqp,Parmeter);
      [X,f,errorcode] = sqp_h(@bsp03a,X,Parsqp,Pardlqp,Parmeter);
      switch I
         case 1, save daten03a X Parmeter;
         case 2, save daten03b X Parmeter;
         case 3, save daten03c X Parmeter;
         case 4, save daten03d X Parmeter;
         case 5, save daten03e X Parmeter;
      end
      disp(' ---------------- ')
   end
   load daten03e X; FAKTOR = 2; Parmeter(8) = FAKTOR;
   [X,f,errorcode] = sqp_h(@bsp03,X,Parsqp,Pardlqp,Parmeter);
   save daten03f X Parmeter
   fig0407
case 4, disp(' Servo Problem Burges-Graham, p. 281 ')
   % with transformation, T_END is control parameter
   startwert = 100;
   while ~ismember(startwert,[1,2,3,4])
      startwert   = input(' Which initial value? (1/2/3/4) ');
   end;
   n     = 40;      % Number of time intervals
   a     = 0;       % Damping
   omga  = 1;       % sqrt(omga) Frequence
   Maxit = 40; epsilon = 4;
   acceleration = 1; BeginMaratos = 20;
   %acceleration 2 and 3 fails 
   switch startwert
   case 1, X0 = [0;-6];   % Initial point
      UN = ones(n+1,1);   % Nominal control
      GG = 1; FF = 1;     % Additional Weights
   case 2, X0 = [-6; 0]; UN = - ones(n+1,1); 
      GG = 1; FF = 1;     
   case 3, X0 = [0; 6]; UN = -ones(n+1,1);
      GG = 1; FF = 1;     
   case 4, X0 = [6; 0]; UN = ones(n+1,1); 
      GG = 1; FF = 10;   
   end
   Tol = 1.0E-12;  %  stop tolerance
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n;a;omga;X0;GG;FF];
   XN1 = ones(n+1,1);      % Nominal state X
   XN2 = zeros(n+1,1);     % Nominal state Y
   TN  = 5;                % Nominal time
   X   = [XN1;XN2;UN;TN];
  % [X,f,errorcode] = sqp(@bsp04,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp(@bsp04a,X,Parsqp,Parmeter);
   switch startwert
   case 1, save daten04a X Parmeter
   case 2, save daten04b X Parmeter
   case 3, save daten04c X Parmeter
   case 4, save daten04d X Parmeter
   end
   fig0408a(startwert)
case 5, disp(' Hartl et al. SIAM REVIEW 37, S. 204 ')
   Start = 1; % Start = 1/2/3
   Maxit = 80; n = 60; X0 = 1; T_END = 3; Tol = 1E-9; 
   epsilon  = 4; acceleration   = 1; BeginMaratos = 90;
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n;X0;T_END];
   switch Start
   case 1,
      XN = ones(n+1,1);    % Nominal state
      UN = zeros(n+1,1);   % Nominal control
      n1 = floor(n/2); n2 = n1 + 1;
      UN(1:n1) = - 1; UN(n2:n+1) = 1; X = [XN;UN];
   case 2
      % Mittelwerte fuer X bilden
      load daten05 X Parmeter
      XN = X(1:n+1);
      V  = X(n+2:2*(n+1));
      UN = [V(1);(V(2:n) + V(1:n-1))/2;V(n+1)];
      X  = [XN;UN];
   case 3, load daten05 X Parmeter
   end
   %[X,f,errorcode] = sqp(@bsp05,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp(@bsp05a,X,Parsqp,Parmeter);
   save daten05 X Parmeter
   % form mean values for  X
   load daten05 X Parmeter
   XN = X(1:n+1);
   V  = X(n+2:2*(n+1));
   UN = [V(1);(V(2:n) + V(1:n-1))/2;V(n+1)];
   X  = [XN;UN];
   save daten05 X Parmeter
   fig0409
   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
case 6, disp(' Hartl et al. SIAM REVIEW 37, p. 207 ')
   Start = 1;
   Maxit = 150; n = 60; r = 1; Tol = 1.0E-9; T_END = 3;
   epsilon  = 4; 
   acceleration = 1; BeginMaratos = 160;
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n;r;T_END];
   if Start == 1
      XN = ones(n+1,1);     % Nominal state
      UN = 0.5*ones(n+1,1); % Nominal control
      X  = [XN;UN];
   end
   if Start == 0, load daten06 X Parmeter, end
  % [X,f,errorcode] = sqp(@bsp06,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp(@bsp06a,X,Parsqp,Parmeter);
   save daten06 X Parmeter
   fig0410
case 7, disp(' Hartl et al. SIAM REVIEW 37, S. 208 ')
   Maxit = 130; n = 40; alfa = -1; Tol = 1.0E-11;
   T_END = 3; epsilon = 4;
   acceleration = 1; BeginMaratos = 140;
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n; alfa; T_END];
   Start = 1;
   if Start == 1
      XN1 = ones(n+1,1);     % Nominal state 1
      XN2 = ones(n+1,1);     % Nominal state 2
      UN  = 0.5*ones(n+1,1); % Nominal control 
      X   = [XN1;XN2;UN];    
   end
   if Start == 0
      load daten07 X Parmeter
   end
  % [X,f,errorcode] = sqp(@bsp07,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp(@bsp07a,X,Parsqp,Parmeter);
   save daten07 X Parmeter
   fig0411
case 8, disp(' Hartl et al. SIAM REVIEW 37, p. 210 ')
   Maxit = 100; n = 40; Tol = 1.0E-13; T_END = 1;
   epsilon  = 4; acceleration = 1; BeginMaratos = 110;
   Parsqp   = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n; T_END];
   Start = 1;
   if Start == 1
      XN = ones(n+1,1);     % Nominal state
      UN = 0.5*ones(n+1,1); % Nominal control
      X  = [XN;UN];
   end
   if Start == 0
      load daten08 X Parmeter
   end
 %  [X,f,errorcode] = sqp(@bsp08,X,Parsqp,Parmeter);
   [X,f,errorcode] = sqp(@bsp08a,X,Parsqp,Parmeter);
   save daten08 X Parmeter
   fig0412
case 9, disp(' Brachistochrone Problem, cf. Bryson-Ho, p. 119 ')
   % Anfangspunkt (0,0)
   n     = 20;          % Number of time intervals
   g     = 0.81;        % grav. acceleration
   B1    = 1;           % terminal point
   Tol   = 1.0E-5;      % estimated tolerance
   T_END = 1;
   Maxit = 40;          % max. step number in SQP
   epsilon = 4;  acceleration = 1; BeginMaratos = 50;
   Parsqp  = [Tol,Maxit,epsilon,acceleration,BeginMaratos];
   Parmeter = [n;g;B1;T_END];
   % trivialer Startwert: lineare Verbindung von A und B
   XN1 = linspace(0,B1,n+1)';  % Nominal state X
   XN2 = linspace(0,B1,n+1)';  % Nominal state Y
   UN  = ones(n+1,1);          % Nominal control
   TN  = 2;                    % Nominal time
   X   = [XN1;XN2;UN;TN];
   for startwert = 1:4
      switch startwert
      case 1, C = 0.4;  Parmeter = [Parmeter(1:4); C];
      case 2, C = 0.2;  Parmeter = [Parmeter(1:4); C];
      case 3, C = 0.15; Parmeter = [Parmeter(1:4); C];
      case 4, C = 0.1;  Parmeter = [Parmeter(1:4); C];
      end
   %  [X,f,errorcode] = sqp(@bsp09,X,Parsqp,Parmeter);
      [X,f,errorcode] = sqp(@bsp09a,X,Parsqp,Parmeter);
      switch startwert
         case 1, save daten09a X Parmeter
         case 2, save daten09b X Parmeter
         case 3, save daten09c X Parmeter
         case 4, save daten09d X Parmeter
      end
   end
   fig0413
end;
disp(' ------------------- ')
if errorcode == 0, disp(' solution ');
else, disp(' Tolerance not achieved ');
   switch errorcode
   case 1, disp(' Max. step number in iteration')
   case 2, disp(' Max. step number in backtracking')
   case 3, disp(' Max. step number in QP_Adaption or QP-IT.')
   end
end;
clear all
