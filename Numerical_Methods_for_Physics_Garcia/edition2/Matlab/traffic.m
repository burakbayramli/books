% traffic - Program to solve the generalized Burger  
% equation for the traffic at a stop light problem
clear all;  help traffic; % Clear memory and print header

%* Select numerical parameters (time step, grid spacing, etc.).
method = menu('Choose a numerical method:', ...
       'FTCS','Lax','Lax-Wendroff');
N = input('Enter the number of grid points: ');
L = 400;      % System size (meters)
h = L/N;      % Grid spacing for periodic boundary conditions
v_max = 25;   % Maximum car speed (m/s)
fprintf('Suggested timestep is %g\n',h/v_max);
tau = input('Enter time step (tau): ');
fprintf('Last car starts moving after %g steps\n', ...
								   (L/4)/(v_max*tau));
nstep = input('Enter number of steps: ');
coeff = tau/(2*h);        % Coefficient used by all schemes
coefflw = tau^2/(2*h^2);  % Coefficient used by Lax-Wendroff

%* Set initial and boundary conditions
rho_max = 1.0;                  % Maximum density
Flow_max = 0.25*rho_max*v_max;  % Maximum Flow
% Initial condition is a square pulse from x = -L/4 to x = 0
rho = zeros(1,N);
for i=round(N/4):round(N/2-1)
  rho(i) = rho_max;    % Max density in the square pulse
end
rho(round(N/2)) = rho_max/2;  % Try running without this line
% Use periodic boundary conditions
ip(1:N) = (1:N)+1;  ip(N) = 1;   % ip = i+1 with periodic b.c.
im(1:N) = (1:N)-1;  im(1) = N;   % im = i-1 with periodic b.c.

%* Initialize plotting variables.
iplot = 1;
xplot = ((1:N)-1/2)*h - L/2;  % Record x scale for plot
rplot(:,1) = rho(:);          % Record the initial state
tplot(1) = 0;
figure(1); clf;  % Clear figure 1 window and bring forward

%* Loop over desired number of steps.
for istep=1:nstep
	
  %* Compute the flow = (Density)*(Velocity)
  Flow = rho .* (v_max*(1 - rho/rho_max));
  
  %* Compute new values of density using FTCS, 
  %  Lax or Lax-Wendroff method.
  if( method == 1 )      %%% FTCS method %%%
    rho(1:N) = rho(1:N) - coeff*(Flow(ip)-Flow(im));
  elseif( method == 2 )  %%% Lax method %%%
    rho(1:N) = .5*(rho(ip)+rho(im)) ...
                   - coeff*(Flow(ip)-Flow(im));
  else                   %%% Lax-Wendroff method %%%
    cp = v_max*(1 - (rho(ip)+rho(1:N))/rho_max);
    cm = v_max*(1 - (rho(1:N)+rho(im))/rho_max);
    rho(1:N) = rho(1:N) - coeff*(Flow(ip)-Flow(im)) ...
             + coefflw*(cp.*(Flow(ip)-Flow(1:N)) ...
                      - cm.*(Flow(1:N)-Flow(im)));
  end

  %* Record density for plotting.
  iplot = iplot+1;
  rplot(:,iplot) = rho(:);
  tplot(iplot) = tau*istep;
  
  %* Display snap-shot of density versus position 
  plot(xplot,rho,'-',xplot,Flow/Flow_max,'--');
  xlabel('x'); ylabel('Density and Flow');
  legend('\rho(x,t)','F(x,t)');
  axis([-L/2, L/2, -0.1, 1.1]);
  drawnow;
end

%* Graph density versus position and time as wire-mesh plot
figure(1); clf;  % Clear figure 1 window and bring forward
mesh(tplot,xplot,rplot)
xlabel('t'); ylabel('x'); zlabel('\rho');
title('Density versus position and time');
view([100 30]);  % Rotate the plot for better view point
pause(1);    % Pause 1 second between plots

%* Graph contours of density versus position and time.
figure(2); clf;   % Clear figure 2 window and bring forward
% Use rot90 function to graph t vs x since
% contour(rplot) graphs x vs t.
clevels = 0:(0.1):1;   % Contour levels
cs = contour(xplot,tplot,flipud(rot90(rplot)),clevels); 
clabel(cs);            % Put labels on contour levels            
xlabel('x');  ylabel('time');  title('Density contours');                    

