function [tout,Tout] = demoSteel(nref,rtol,atol)
% demoSteel  Solve ODE describing heat treating of a steel bar using ode45
%
% Synopsis:  demoSteel                   [t,T] = demoSteel
%            demoSteel(nref)             [t,T] = demoSteel(nref)
%            demoSteel(nref,rtol)        [t,T] = demoSteel(nref,rtol)
%            demoSteel(nref,rtol,atol)   [t,T] = demoSteel(nref,rtol,atol)
%
% Input:     nref = (optional) number of interpolations between steps.
%                    Default:  nref = 4 (internal default used by ode45)
%            rtol = (optional) relative tolerance used by ode45
%                   Default: tol = 1e-3 (internal default used by ode45)
%            atol = (optional) absolute tolerance used by ode45
%                   Default: tol = 1e-6 (internal default used by ode45)
%
% Output:    Plot of temperature of the bar versus time.
%            t = (optional) vector of times at which solution is obtained
%            T = (optional) vector of temperatures from numerical solution

if nargin<1,  nref = 4;     end    %  Process optional input arguments
if nargin<2,  rtol = 1e-3;  end
if nargin<3,  atol = 1e-6;  end

len = 1;     dia = 1e-2;       %  bar length and diameter, meters
rho = 7822;  c = 444;          %  density (kg/m^3) and heat capacity (J/kg/K)
As = pi*dia*len;               %  surface area of bar, m^2, neglect ends
mc = len*0.25*pi*dia^2*rho*c;  %  mc = rho*volume*c
emiss = 0.7;                   %  emissivity of bar
htc = [15; 100];               %  heat transfer coefficients, W/m^2/C
Ta = 21 + 273.15;              %  ambient temperature, K
tcool = 70;                    %  begin cooling at tcool seconds
tf = 3*tcool;                  %  total simulation time, seconds
QV = 3000;                     %  rate of electrical heat generation, W

% --- Set tolerance and refinement options and solve the ODE
options = odeset('RelTol',rtol,'AbsTol',atol,'Refine',nref);
[t,T] = ode45('rhsSteelHeat',tf,Ta,options,mc,QV,tcool,htc,As,Ta,emiss);

T = T - 273.15;               % convert kelvins to Celsius 
f = figure;  plot(t,T,'+');   % open new figure window and plot
xlabel('Time  (s)');  ylabel('Temperature (C)');
title(sprintf('nref = %d,  rtol = %9.1e,  atol = %9.1e',nref,rtol,atol));
fprintf('rtol = %9.2e  atol = %9.2e  Tmax = %7.2f\n',rtol,atol,max(T));

if nargout>1,   tout = t;  Tout = T;  end   %  Optional return variables
