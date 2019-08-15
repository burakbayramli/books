function demo
% Masterfile for test of NEWTON method
% Example 1: Test for NEWTON-method, Hoellig, S. 103
% Boundary value problem with box scheme:
% Example 2: Rotational surface

errorcode = 0;
nr = 100;
while ~ismember(nr,[1,2])
   nr   = input(' Beispiel Nr. (1/2) ');
end;
switch nr
case 1
   e = 0.1;
   X0  = [- pi + e; 3*pi + e];
   tol = 1.0e-05;
   % -----------------------------------------
   [X,errorcode] = newton('bsp01',X0,tol); %--
   % -----------------------------------------
   errorcode
   Residuum = [exp(X(1))*cos(X(2))-1; exp(X(1))*sin(X(2))]
case 2
   global r0 r1 d n G
   r0  = 1;       % Radius left
   r1  = 2;       % Radius right
   d   = 2;       % Order of differential system
   n   = 20;      % Number of intervals
   H   = 1/n;     % Step length
   tol = 0.01;    % Stop tolerance
   G   = 'bsp02a'; % Bsp. Minimale Rotational surface 
   %G   = 'bsp02b'; % Bsp. Minimale Rotational surface,
                   % Gradient approximared
   % ------------------------------------------------------
   % simple start trajectory: linear connection of r0 and r1
   X0 = [ linspace(r0,r1,n+1); (r1-r0)*ones(1,n+1)];
   % ------------------------------------------------------
   %  Solution:
   %  d = -2.383;
   %  T =  0:H:1;
   % X0 = [ cosh(d+T*cosh(d))/cosh(d) ; sinh(d+T*cosh(d)) ];
   % -------------------------------------------------------
   X0  = X0(:);
   % ----------------------------------------
   [X,errorcode] = newton('box',X0,tol);%----
   % ----------------------------------------
   X   = reshape(X,d,n+1);
   save daten H X
   % 3D-Plot of solution
   bild2
end
