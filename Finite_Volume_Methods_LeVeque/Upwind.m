%% 1D Linear Advection Equation
%
% du/dt + a du/dx = 0
% https://github.com/wme7/Aero-matlab/blob/master/CFD/Upwind.m
% Ref's: 
%   Randall J. Leveque;
%   Finite Volume Methods for Hyperbolic Problems
%   Cambridge Press, 2002.
%
% Subroutine for solving using Upwind Method.
% by Manuel Diaz, manuel.ade'at'gmail.com 
% Institute of Applied Mechanics, 2012.08.12

clear all; close all; clc;

%% Parameters
a = 0.5;
a_m = min(0,a);
a_p = max(0,a);
dx = 0.01;
cfl = 0.9;
dt = cfl*dx/abs(a);
t_end = 0.6;

%% Discretization of Domain
x = 1:dx:2;
t = 0:dt:t_end;

%% Initial Condition
n = length(x);
%u_0 = zeros(1,n);
u0(1:ceil(n/2)) = 1;
u0((ceil(n/2)+1):n) = 2;

%% Main Loop
% Initilize vector variables
u_next = zeros(size(u0)); % u^{n+1}
u_bar1 = zeros(size(u0)); % preallocate for MacCormack
u_bar2 = zeros(size(u0)); % preallocate for MacCormack

% Load Initial condition
u = u0;

% Main Loop
for k = t
    % plot every time step
    plot(x,u,'.')
    
    for j = 2:n-1    
        % Single Sided Upwind
        %u_next(j) = u(j) - a*dt/dx*(u(j)-u(j-1)); % Upwind
        %u_next(j) = u(j) - a*dt/dx*(u(j+1)-u(j)); % Downwind
        
        % Double Sided Upwind
        u_next(j) = u(j) - a_p*dt/dx*(u(j)-u(j-1)) - a_m*dt/dx*(u(j+1)-u(j));
        
        % Comparing to Lax-Wendroff:
        %u_next(j) = u(j) - a*dt/(2*dx)*(u(j+1)-u(j-1)) ...
        %   +(dt^2/2)/(dx^2)*(a^2)*(u(j+1)-2*u(j)+u(j-1)); % LW
        
        % Comparing to MacCormack Method:
        % Predictor steps
        %u_bar1(j) = u(j) - a*dt/dx*(u(j+1)-u(j)); 
        %    u_bar1(1) = u_bar1(2); % Neumann BC in predictor step
        %u_bar2(j) = u_bar1(j) - a*dt/dx*(u_bar1(j)-u_bar1(j-1)); 
        % Corrector step
        %u_next(j) = 1/2*(u(j)+u_bar2(j));
    end
    % BC
    u_next(1) = u_next(2);      % Neumann BC
    u_next(n) = u_next(n-1);    % Neumann BC
    
    % UPDATE info
    u = u_next(1:n);
    
    % update figure
    pause(0.05); drawnow
end
    
%% Plot final Result
plot(x,u,'.')
