function demo3
% Eckart Gekeler, Universitaet Stuttgart, Release 30.4.07
% Periodic solutions of Duffing equation
% x'' + 2ax' + x^3 = b*cos(t), transformed to
% parameter dependent problem with interval length = 1
% Approximative solution as initial value problem 
clc, format short, format compact
%clf
n  = 2;            % Dimension of system
G  = @bsp04;      %  Current example
for FALL = 1:5
switch FALL
case 1 
   m = 200;
   Periode   = 9.1;          % Estimated period
   ANF       = [2.72;0];      % Estimated initial vector for IVP
   a = 0.1; b = 5;            % parameter of Duffing equation 
   Parmeter3 = [Periode,a,b]; % Parameter for example
case 2 
   m = 200; Periode = 9; ANF = [2.72;0];
   a = 0.1; b = 4.5;  Parmeter3 = [Periode,a,b]; 
case 3 
   m = 200; Periode = 8.8; ANF = [2.72;0];
   a = 0.1; b = 4;  Parmeter3 = [Periode,a,b]; 
case 4 
   m = 200; Periode = 14; ANF = [2.72;0];
   a = 0.1; b = 3.5;  Parmeter3 = [Periode,a,b]; 
case 5 % see Seydel 94, p. 214.
   G =@bsp05;
   m = 1000; Periode = 118.55; ANF = [1.39;0];
   a = -0.04; b = 0.2; c = -8/15; d = 0.4;  
   Parmeter3 = [Periode,a,b,c,d]; 
end
Flag    = 1; % only function value used
options = odeset('reltol',1.0E-6,'abstol',1.0E-3);
TT      = linspace(0,1,m+1);   
[TA,X0] = ode23(G,TT,ANF,options,Flag,Parmeter3);
X = X0.'; XA = X;
save datend m XA Periode
fig0224
pause
end
clear
