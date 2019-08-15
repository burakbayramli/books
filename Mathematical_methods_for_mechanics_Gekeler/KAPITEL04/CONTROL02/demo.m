function demo
% Eckart Gekeler, Universitaet Stuttgart, Release 21.05.08
% Masterfile Control problems with NEWTON's method
% Example 1:  Thrust problem, Bryson-Ho, p. 59
%             a(t) = 1, T = 1, Homotopy for altitude
%             gradient becomes quasisingular above altitude 0.4
% Example 2:  Orbit problem, Bryson-Ho, p. 66
%             Mass of spacecraft constant
%             Transformation into dimensfree system
% Example 3:  ZERMELO's problem, cf. Bryson-Ho, p. 76 ff.
%             GOH-TEO-Transformation
%             velocity of ship S = 1, current velocity = [-S/2,0]
% -----------------------------------------------
clc, format short, format compact
nr = 100; errorcode = 0;
while ~ismember(nr,[1,2,3])
   nr = input(' Example No. (1/2/3) ');
end;
% -----------------------------------------------
switch nr
case 1, disp(' Thrust-Problem ')
   Maxit     = 15;           % Max. step no. in Newton method
   tol       = 0.01;         % Stop tolerance
   Parmeter1 = [Maxit, tol]; % Parameter for Newton method
   % --------------------------------------------
   G = @bsp01;  % The current example
   G = @bsp01a; % The current example, calculated derivatives
   d = 8;       % Dimension of system
   n = 20;      % Number of time intervals
   T = 1;       % Operational time
   Parmeter2 = [d,n,T]; % Parameter for box scheme
   % --------------------------------------------
   Height    = 0;
   Parmeter3 = Height;        % Parmeter for problem
   % Calculation of start trajectory ------------
   AW = [0; 0]; % initial point
   BW = [1; 0]; % terminal point
   b  = 0;      % angle of ship's axis
                % in straight flight from AW to BW
   % trivial start trajectory: straight line between AW and BW
   X0 = zeros(8,n+1);       X0(1,:) = linspace(0,1,n+1)*0.5;
   X0(2,:) = zeros(1,n+1);  X0(3,:) = linspace(0,1,n+1);
   X0(4,:) = sin(b)*ones(1,n+1); X0(5,:) = zeros(1,n+1);
   X0(6,:) = zeros(1,n+1);  X0(7,:) = ones(1,n+1);
   X0(8,:) = tan(b)*ones(1,n+1); X0 = X0(:);
   % --------------------------------------------
   for ITER = 1:3
       ITER
       % ----------------------------------------
       [X,errorcode] = newton(@box,X0,G,Parmeter1,Parmeter2,Parmeter3);
       % ----------------------------------------
       X0 = X; Height = Height + 0.1; Parmeter3 = Height;
    end
    % -------------------------------------------
    X = reshape(X,d,n+1);
    save daten1 X n, bild01
case 2, disp(' Orbit-Problem ')
   Maxit     = 15;           % Max. step number in Newton method
   tol       = 0.005;        % Stop tolerance
   Parmeter1 = [Maxit, tol]; % Parameter for Newton method
   % -------------------------------------------------
   G = @bsp02;  % The current example
   G = @bsp02a; % The current example, calculated derivatives
   d = 6;       % Dimension of system
   n = 20;      % Number of time intervals
   T = 2;       % Operational time
   Parmeter2 = [d, n, T];    % Parameter for box scheme
   % -------------------------------------------------
   KAPPA     = 0.14;         % A constant of the system
   PP        = 20;           % Additional weight in terminal payoff
   Parmeter3 = [KAPPA, PP];  % Parameter for problem
   % Calculation of start trajectory -----------------
   Y2 = 0; Y3 = 1; Y = bsp02b(n,T,Y2,Y3,Parmeter3);
   X0      = zeros(6,n+1); X0(1,:) = ones(1,n+1);
   X0(3,:) = ones(1,n+1); X0(4:6,:) = Y;  X0 = X0(:);
   % ---------------------------------------------
   [X,errorcode] = newton(@box,X0,G,Parmeter1,Parmeter2,Parmeter3);
   % ---------------------------------------------
   X = reshape(X,d,n+1);
   save daten2 X T, bild02
   Y1_T = X(4,n+1), Y2_T = X(5,n+1), Y3_T = X(6,n+1)
case 3, disp(' Problem of Zermelo ')
   Maxit     = 15;           % Max. step number in Newton method
   tol       = 0.01;         % Stop tolerance
   Parmeter1 = [Maxit, tol]; % Parameter for Newton method
   % -----------------------------------------------------
   G = @bsp03;  %  The current example
   G = @bsp03a; %  The current example, calculated derivatives
   d = 4;       % Dimension of the system
   n = 20;      % Number of timeintervals
   T = 1;       % Operational time
   Parmeter2 = [d,n,T];    % Parameter for box scheme
   % -----------------------------------------------------
   AW = [3.61;-1.8];         % Initial point
   %AW = [4;-2];         % initial point
   BW = [0;0];               % Terminal point
   S  = 1;                   % Velocity of ship 
   Parmeter3 = [AW;BW;S];  % Parameter for problem
   % Start trajectory: linear connection between AW and BW
   % with angle a of ship's axis
   a  = bsp03b(AW,BW,S); % angle of ship's axis
                         % for flow velocity v = [-S/2; 0]
                         % and straight connection between AW and BW
   X0 = [linspace(AW(1),BW(1),n+1); linspace(AW(2),BW(2),n+1);
         ones(1,n+1); ones(1,n+1)*a];
   X0 = X0(:);
   % ---------------------------------------------
   [X,errorcode] = newton(@box,X0,G,Parmeter1,Parmeter2,Parmeter3);
   % ---------------------------------------------
   X = reshape(X,d,n+1);
   Operation_time = X(3,1)
   save daten3 X Parmeter2, bild03
end
disp(' ------------------- ')
if errorcode == 0
   disp(' Solution ');
else
   disp(' No or bad solution '); tol, Maxit
   if errorcode == 1, disp(' Max. step number '), end
   if errorcode == 2, disp(' Max. step number in backtracking ')
   end
end;
