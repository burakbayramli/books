% Driver script for solving the 1D Euler equations using a flux-limit scheme 
clear all

% Set problem parameters
L = 1; FinalTime = 0.2; N = 1024; h = L/N; CFL = 0.90; gamma= 1.4;

% Define domain, materials and initial conditions
r = zeros(N+1,1); ru = zeros(N+1,1); E = zeros(N+1,1);

% Initialize for Sod's problem
x = [0:h:1]';
for i=1:N+1;
    if x(i)<0.5
        r(i)=1.0; E(i) = 1/(gamma-1);
    else
        r(i) = 0.125; E(i) = 0.1/(gamma-1);
    end
end

% Initialize for shock entropy problem
% x = [-5:h:5]';
% for i=1:N+1;
%      if x(i)<-4
%          rh = 3.857143; u = 2.629369; p = 10.33333;   
%      else
%          rh = 1+0.2*sin(pi*x(i)); u = 0; p = 1;
%      end
%      r(i) = rh; ru(i) = rh*u; E(i) = p/(gamma-1)+0.5*rh*u^2;
%  end

% Solve Problem
q = [r ru E];
[q] = EulerFL1D(x,q,h,CFL,gamma,FinalTime);