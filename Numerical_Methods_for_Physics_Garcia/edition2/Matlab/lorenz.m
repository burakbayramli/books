% lorenz - Program to compute the trajectories of the Lorenz 
% equations using the adaptive Runge-Kutta method.
clear;  help lorenz;

%* Set initial state x,y,z and parameters r,sigma,b
state = input('Enter the initial position [x y z]: ');
r = input('Enter the parameter r: '); 
sigma = 10.;   % Parameter sigma
b = 8./3.;     % Parameter b
param = [r sigma b];  % Vector of parameters passed to rka
tau = 1;       % Initial guess for the timestep
err = 1.e-3;   % Error tolerance

%* Loop over the desired number of steps
time = 0;
nstep = input('Enter number of steps: ');
for istep=1:nstep

  %* Record values for plotting
  x = state(1); y = state(2); z = state(3);
  tplot(istep) = time;  tauplot(istep) = tau;       
  xplot(istep) = x;  yplot(istep) = y;  zplot(istep) = z;
  if( rem(istep,50) < 1 )
    fprintf('Finished %g steps out of %g\n',istep,nstep);
  end

  %* Find new state using adaptive Runge-Kutta
  [state, time, tau] = rka(state,time,tau,err,'lorzrk',param);
  
end

%* Print max and min time step returned by rka
fprintf('Adaptive time step: Max = %g,  Min = %g \n', ...
           max(tauplot(2:nstep)), min(tauplot(2:nstep)));

%* Graph the time series x(t)
figure(1); clf;  % Clear figure 1 window and bring forward
plot(tplot,xplot,'-')
xlabel('Time');  ylabel('x(t)')
title('Lorenz model time series')
pause(1)  % Pause 1 second

%* Graph the x,y,z phase space trajectory
figure(2); clf;  % Clear figure 2 window and bring forward
% Mark the location of the three steady states
x_ss(1) = 0;              y_ss(1) = 0;       z_ss(1) = 0;
x_ss(2) = sqrt(b*(r-1));  y_ss(2) = x_ss(2); z_ss(2) = r-1;
x_ss(3) = -sqrt(b*(r-1)); y_ss(3) = x_ss(3); z_ss(3) = r-1;
plot3(xplot,yplot,zplot,'-',x_ss,y_ss,z_ss,'*')
view([30 20]);  % Rotate to get a better view 
grid;           % Add a grid to aid perspective
xlabel('x'); ylabel('y'); zlabel('z');
title('Lorenz model phase space');
