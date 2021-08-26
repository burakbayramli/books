% Driver script for solving the 1D Euler equations using an ENO scheme 
clear all

% Order of method
m=2;

% Set problem parameters
L = 1; FinalTime = 0.2; N = 256; CFL = 0.90; gamma= 1.4;  h = L/N; 

% Define domain, materials and initial conditions
r = zeros(N+1,1); ru = zeros(N+1,1); E = zeros(N+1,1);

% Initialize for Sod's problem - piecewise constant so no integration
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
% NGQ = 10; [xGQ,wGQ] = LegendreGQ(NGQ);
% for i=1:N+1;
%    if x(i)<-4
%        rh = 3.857143; u = 2.629369; p = 10.33333;   
%    else
%        rh = 0;
%        for j=1:NGQ+1
%            rh = rh + wGQ(j)*(1+0.2*sin(pi*(x(i)+h/2*xGQ(j))));
%        end 
%        rh = rh/2; u = 0; p = 1;
%    end
%    r(i) = rh; ru(i) = rh*u; E(i) = p/(gamma-1)+0.5*rh*u^2;
% end

% Solve Problem
q = [r ru E];
[q] = EulerENO1D(x,q,h,m,CFL,gamma,FinalTime);