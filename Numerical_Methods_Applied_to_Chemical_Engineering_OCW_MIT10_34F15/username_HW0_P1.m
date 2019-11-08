function username_HW0_P1
%Much of this written by: Vibhu on Aug 20, 2009
% and modified by Bill Green Sept 3, 2010
% and finished and tested by Jason Moore on Sept 15, 2010
% and modified for Fall 2014 course by Liza Lee on Sept 3, 2014
%This function solves a second order differential equation by integrating
%   two first order coupled differential equations obtained from the original
%   second order differential equation, see 10.34 problem statement
%Inputs: None
%Outputs: Graph of temperature and temperature gradient vs. radius of the
%   cylinder as asked in the problem 1 of HW1

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
[]=ode23(); % FILL IN THE INPUTS AND OUTPUTS HERE

%Plot the temperature and temperature gradient vs. radius of the cylinder
% INCLUDE YOUR PLOTTING FUNCTION HERE

%//////////////////////////////////////////////////////////////////////////

function udash=dudy(y,u,param)
%Written by: Vibhu on Aug 20, 2009
% and modified by Jason Moore Sept 15,2010
%This function calculates du/dr for the second order differential
%   equation for temperature
%                   d2T/dr2+(1/r)dT/dr+(q/k)=0
%   by converting it to two first order coupled differential equations
%                   u1=T
%                   u2=dT/dr
%Inputs: FILL IN THE INPUT(S)
%
%Outputs: FILL IN THE OUTPUT(S)
%

%Extract parameters:
R = param(1);
qdot = param(2);
%Intialize the output vector to zero
udash = zeros(size(u));
%Evaluate k(T):
k = 148*(u(1)./300).^(-1.3);
%Fill the values for output vector
udash(1) = -u(2);
udash(2) = ; % FILL IN THE RIGHT HAND SIDE

%//////////////////////////////////////////////////////////////////////////
% WRITE YOUR PLOTTING FUNCTION HERE
function 

