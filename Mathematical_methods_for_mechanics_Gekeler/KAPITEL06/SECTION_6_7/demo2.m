function demo2
% demo for top
% 8 examples for 8 different cases
% Plots V_EFF and DOTPHI
% the function  f(x) = (alfa - beta*x)*(1 - x^2) - (a - b*x)^2
% must have roots within  [-1, 1] in cases 1,2,3
% a = d3/T1, b = D3/T1, alfa = 2e'/T1, beta = 2mgl/T1
% Physical data only for beta > 0 !!

clear all, clc, format compact, format short
T1 = 1; T3 = 2; gl = 0; m = 1; tol = 1E-3;
Parmeter = [T1, T3, m, gl, tol]; T = []; Y = [];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
done      = 0;
K         = [1 2 3 4 5 6 7 8 9];
while ~done
   nr   = input(' Example No. (1/2/3/5/6/7/8/9) ');
   done = ismember(nr,K);
end;
switch nr
case 1, disp(' Case 1 ')
   d3 = 3; D3 = 2; theta1 = pi/6; theta2 = pi/2;
   [a,b,alfa,beta] = f_koeff(d3,D3,theta1,theta2,Parmeter);
   Parmeter(4) = beta/2; % = gl
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0627a
case 2, disp(' Case 2 ')
   d3 = - 3; D3 = 1; theta1 = pi/2; theta2 = 3*pi/4;
   [a,b,alfa,beta] = f_koeff(d3,D3,theta1,theta2,Parmeter);
   Parmeter(4) = beta/2; % = gl
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0627b
case 3, disp(' Case 3 ')
   d3 = sqrt(3); D3 = 2; theta1 = pi/6; theta2 = pi/2;
   [a,b,alfa,beta] = f_koeff(d3,D3,theta1,theta2,Parmeter);
   Parmeter(4) = beta/2; % = gl
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0627c
case 4 % Case 4 does not occur here!
   disp('Unphysical Data!')
   return
case 5, disp(' Case 5 ')
   d3 = 1; D3 = 2; theta1 = pi/4; theta2 = pi/2;
   [a,b,alfa,beta] = f_koeff(d3,D3,theta1,theta2,Parmeter);
   Parmeter(4) = beta/2; % = gl
   gl = beta/2; Parmeter(4) = gl;
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0627d
case 6, disp(' Case 6 ')
   d3 = 1.2; D3 = 1; theta1 = pi/4; theta2 = pi/4;
   [a,b,alfa,beta] = f_koeff(d3,D3,theta1,theta2,Parmeter);
   Parmeter(4) = beta/2; % = gl
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0627e
case 7, disp(' Case 7 ')
   d3 = -1; D3 = 1; theta1 = pi/2; theta2 = pi;
   a = d3/T1; b = D3/T1; alfa = a*a; beta = 2;
   % beta free!
   Parmeter(4) = beta/2; % = gl
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0627f
case 8, disp(' Case 8 ')
   d3 = 3; D3 = 3; theta1 = 0; theta2 = pi/2;
   a  = 3; b = 3; alfa = a*a; beta = 1;
   % beta free!
   Parmeter(4) = beta/2; % = gl
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0628a
case 9, disp(' Case 9 ')
   d3 = 1; D3 = 1; theta1 = 0; theta2 = pi/2;
   a  = 1; b = 1; alfa = a*a; beta = 1;
   Parmeter(4) = beta/2; % = gl
   a_b_alfa_beta = [a, b, alfa, beta]
   save daten Parmeter theta1 theta2 d3 D3
   fig0628b
end
