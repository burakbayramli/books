function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 23/05/08
% Gradient method for control problems
% Example 1: Dyer-McReynolds, p. 127
% Example 2: Dyer-McReynolds, p. 128
%            Brachistochrone problem
% Example 3: Orbit problem, Bryson-Ho, p. 66, Dyer, p. 73
% Example 4: Thrust problem, Bryson_ho, Par. 2.4

clc, clear, format short, format compact
nr = 100;
while ~ismember(nr,[1,2,3,4])
   nr = input(' Example no. (1/2/3/4) ');
end   
% ------------------------------
global d n t0 t1 U X Y 
% ------------------------------
switch nr
case 1
   d  = 2;           % Dimension of differential system
   t0 = 0;           % Initial time
   t1 = 1;           % Terminal time
   n  = 20;          % Number of time intervals
   tol = 1.0E-04;    % Estimated tolerance
   X = zeros(d,n+1); % State vector
   Y = X;            % Costate vector
   U = zeros(1,n+1); % Nominal control
   X0 = [2;-1]; X1 = [0; 0];
   [T,X,U,errorcode] = grad01('bsp01',X0,X1,tol); 
   save daten1 T X U
   bild01
case 2   
   d = 1; t0 = 0; t1 = 1; n = 100;
   n1 = 50; % for initial control U (step function)
   tol = 1.0E-04; 
   X = zeros(d,n+1); Y = X;
   U = zeros(1,n+1); U(n1+1:n+1) = - 0.5;
   X0 = 0; X1 = 0;
   [T,X,U,errorcode] = grad02('bsp02',X0,X1,tol); 
   save daten2 T X U
   bild02
case 3
   global kappa
   d = 3; t1 = 2; n = 20; tol = 1.0E-4; 
   kappa = 0.1405;  % System constant
   X = zeros(d,n+1); Y = X; Y1 = X; Y2 = X;
   c  = - pi + 0.2; % Constant for nominal control
   n1 = floor(n/2); % Constant for nominal control
   U = zeros(1,n+1); 
   U(n1:n+1) = c;  % Nominal control
   RD = bsp03_h(5,Y,Y1,Y2); % Specify NAME !
   X0 = RD(:,1); YT = RD(:,2);
   errorcode = grad03('bsp03',X0,YT,tol,Y,Y1,Y2); 
   save daten3 t1 X U
   bild03
case 4
   global h kappa
   d = 4; t1 = 1; n = 20; tol = 1.0E-8; 
   kappa = 1;   % System constant
   h = 0.2; % Height
   X = zeros(d,n+1); Y = X;
   U = 0.4*ones(1,n+1); % Nominalkontrolle
   %load datenu U
   [X1,Y1,errorcode] = grad04('bsp04a',tol,X,Y); %------
   U1 = U;
   save daten4 X1 Y1 U1
   bild04a
   disp(' Nominal control calculated, see figure ')
   pause
   load daten4 X1 Y1 U1
   U = U1;
   [X2,Y2,errorcode] = grad04('bsp04b',tol,X1,Y1); 
   U2 = U;
   save daten5 X2 Y2 U2
   bild04b
end
switch errorcode
   case 0, disp('Success')
   case 1, disp('Max. step number')
   case 2, disp('Max. step number in backtracking')
end   

   
