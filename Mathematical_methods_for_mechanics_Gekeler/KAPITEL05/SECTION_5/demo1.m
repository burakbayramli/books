function demo1
% Program for simple Hopf bifurcation
% omega*u' + A*u + mu*B*u + f(mu,u) = 0, u(0) = u(2*pi),
% f(mu,0) = 0, B = 0 and mu = 0 allowed
% Method 1: backward differences and conjugate gradients (cg)
%              (works also without cg as in method 2)
% Method 2: Trig. collocation without conjugate gradients
% Choose method below by mutual de-commentation !
% Continuation for trig. collocation: TODO-TODO !!!!
% FUNCTIONS: bdf.m, conjgrad.m, hopf_bdf.m, hopf_trig.m
%            trig_koll.m
% USER DEFINED FUNCTIONS: bsp0x.m
%
clear, clc, format short, format compact
errorcode = 0; nr = 100; KK = [1,2,3,4,5,6,7];
while ~ismember(nr,KK)
   nr   = input(' Example no. (1/2/3/4/5/6/7)? ');
end;
switch nr
case 1, disp(' Van der Pol equation ')
   F     = 'bsp01'; % data of specific eexample
   r     = 64;   % number of equidist. time intervals, r = 2^m
   tol   = 1E-8; % Tolerance
   maxit = 20;   % Max. step number in Newton method
   Eps   = 0.5;  % Procedural parameter to CHOOSE!
   zeta1 = 0.5;  % Start direction, 0 <= zeta1 <= 1
   % supplies only phase translation because rotat. invariance
   Parmeter = []; % Parameter for Example
case 2, disp(' Feedback inhibition model '), F = 'bsp02';
   r = 64; tol = 1E-8; maxit = 5; Eps   = 0.2; zeta1 = 0.5;
   Parmeter = []; % Parameter for Examplel
case 3, disp(' Small brusselator '), F = 'bsp03';
   r = 128; tol = 1E-8; maxit = 10; Eps = 0.5; zeta1 = 0.5;  
   Parmeter = []; % Parameter fuer Beispiel
case 4, disp(' Full Brusselator '), F = 'bsp04';
   r = 128; tol = 1E-8; maxit = 30; Eps   = 0.5; zeta1 = 0.5;  
   a0 = (9 - sqrt(17))/4; Y0 = [1;a0;a0];
   Parmeter = [a0;Y0]; % Parameter for Example
case 5, disp(' Lorentz equation '), F = 'bsp05';
   r = 64; tol = 1E-8; maxit = 5; Eps = 4; zeta1 = 0.5; 
   P = 16; b = 4; R0 = 368/11; S = sqrt(b*R0 - b);
   Y0 = [S; S; R0-1];
   Parmeter = [P; b; R0; S; Y0]; % Parameter for Example
case 6, disp(' Periodic Hamilton system, no Hopf bif. ') 
   F = 'bsp06';
   r = 64; tol = 1E-8; maxit = 10; Eps = 0.4; zeta1 = 0.5;
   Parmeter = [0,0,0,0]; % Parameter for Example
case 7, disp('Test example ') 
   F = 'bsp07';
   r = 64; tol = 1E-8; maxit = 10; Eps = 0.4; zeta1 = 0.5;
   Parmeter = [0,0,0,0]; % Parameter for Example
end
Parhopf = [r,tol,maxit,Eps,zeta1];
%[Y,omga,mu,errorcode] = hopf_bdf1(F,Parhopf,Parmeter);
 [Y,omga,mu,errorcode] = hopf_trig1(F,Parhopf,Parmeter);
switch nr
case 1, save datenB1 Y omga mu Parmeter
   clf, Y = [Y,Y(:,1)]; plot(Y(1,:),Y(2,:))
   axis equal, grid on
case 2, save datenB2 Y omga mu Parmeter
   clf, Y = [Y,Y(:,1)]; plot3(Y(1,:),Y(2,:),Y(3,:)),
   axis equal, grid on
case 3, save datenB3 Y omga mu Parmeter
   clf, Y = [Y,Y(:,1)]; plot(Y(1,:),Y(2,:))
   axis equal, grid on
case 4, save datenB4 Y omga mu Parmeter
   clf, Y = [Y,Y(:,1)];  Y00 = Y0*ones(1,size(Y,2));
   Y = Y00 + Y; plot3(Y(1,:),Y(2,:),Y(3,:)),
   axis equal, grid on
case 5, save datenB5 Y omga mu Parmeter
   clf, Y = [Y,Y(:,1)];  plot3(Y(1,:),Y(2,:),Y(3,:))
   axis equal, grid on
case 6, save datenB6 Y omga mu Parmeter
   clf, Y = [Y,Y(:,1)];  plot(Y(1,:),Y(2,:))
   axis equal, grid on
case 7, save datenB7 Y omga mu Parmeter
   clf, Y = [Y,Y(:,1)];  plot(Y(1,:),Y(2,:))
   axis equal, grid on
end
omga_mu = [omga,mu]

