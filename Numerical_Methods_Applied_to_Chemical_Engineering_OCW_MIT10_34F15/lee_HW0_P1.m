function lee_HW0_P1
%Much of this written by: Vibhu on Aug 20, 2009
% and modified by Bill Green and Jason Moore Sept 3, 2010, 
% Final version by Liza Lee, 2014
%This function solves a second order differential equation by integrating
%   two first order coupled differential equations obtained from the original
%   second order differential equation, see 10.34 problem statement
%Inputs: None
%Outputs: Graph of temperature and temperature gradient vs. radius of the
%   cylinder as asked in the problem 0 of HW1

%Define the limits of the radius from centre to circumference for the
%MATLAB in built integration function ode23()
R=0.2;  % radius of cylinder (m)
delta=0.01; % integrate close to center of cylinder, but not too close
% or might encounter divide by zero error, see dudy() below.
y1=0; % y=zero is the surface of the cylinder
y2=R-delta; % (m)
ylimits=[y1, y2];
qdot= 20000; % (W/m^3)
Tsurface =393;  % (K)
k = 148*(Tsurface./300).^(-1.3);  % (W/m-K)
%Defining the initial conditions for the first order coupled differential
%   equations
u0=[Tsurface, -0.5*R*qdot/k]; % Units in [K, K/m]
%Define necessary parameters for subfunctions
param = [R,qdot];

% Solve the first order coupled differential equations using in built
%   MATLAB function ode23()
[y U]=ode23(@(y,U)dudy(y,U,param),ylimits,u0);
%[y U]=ode23(@(y,U) dudy,ylimits,u0);

%Plot the temperature and temperature gradient vs. radius of the cylinder
pretty_plots(y,U,param)

%//////////////////////////////////////////////////////////////////////////

function udash=dudy(y,u,param)
%Written by: Vibhu on Aug 20, 2009
% and modified by Jason Moore Sept 15,2010
%This function calculates du/dr for the second order differential
%   equation for temperature
%       d2T/dr2+(1/r)dT/dr+(q/k)=0
%   by converting it to two first order coupled differential equations
%       u1=T
%       u2=dT/dr
%Inputs:
%   y - a 1x1 vector containing a point from 0 to R-delta 
%   u - a 2x1 vector containing values of u1 and u2 at y
%   parameter - a 1x2 vector containing the parameters in the order of [R,qdot]
%       R - radius of cylinder (m)
%       qdot - Heat generated per unit volume within the cylinder (KW/m3)
%   k - Thermal Conductivity (W/(m-K)        
%Outputs: 
%   udash - a 2x1 vector containing the values [du1/dy, du2/dy] in [K/m, K/m^2]

%Extract parameters:
R = param(1);
qdot = param(2);
%Intialize the output vector to zero
udash = zeros(size(u));
%Evaluate k(T):
k = 148*(u(1)./300).^(-1.3);
%Fill the values for output vector
udash(1) = -u(2);
udash(2) = u(2)/(R-y) + qdot./k;
end
%//////////////////////////////////////////////////////////////////////////

function pretty_plots(y,U,param)
% by Bill Green 9/3/2010 based on code by Vibhu
% plots T(r) and dT/dr vs. r
% inputs:
%  y is a column vector of position points (meters): y=R-r
%  U is a 2-column matrix; first column is T in Kelvin; 2nd column is dT/dr
% output : 
%  A figure displaying T vs r and dT/dr vs r
R=param(1);
FontSize = 14; % Define font size
subplot(2,1,1) % Using subplot to plot the temperature and temperature
%gradient on the same window in different graphs
plot(R-y,U(:,1),'r','Linewidth',1.5); % Plotting temperature in red by 
% giving input 'r' to plot()
xlabel('Radius (m)', 'FontSize',FontSize); % Labeling the x-axis as Radius
% for subplot 1
ylabel('Temperature (^oC)', 'FontSize',FontSize); % Labeling the y-axis as
% temperature for subplot 1
title('Solving the equation for temperature in a cylinder with heat source',...
    'FontSize',FontSize); % Title the plot
legend('Temperature');

subplot(2,1,2)
plot(R-y,U(:,2),'b','Linewidth',1.5); % Plotting temperature gradient in
% blue by giving input 'b' to plot
xlabel('Radius (m)', 'FontSize',FontSize); % Labeling the x-axis as Radius
% for subplot 2
ylabel('Temperature Gradient (^oK/m)', 'FontSize',FontSize); % Labeling 
% the y-axis as temperature gradient for subplot 2
legend('Temperature Gradient');
end
end