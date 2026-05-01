% pendul - Program to compute the motion of a simple pendulum
% using the Euler or Verlet method
clear all;  help pendul      % Clear the memory and print header

%* Select the numerical method to use: Euler or Verlet
NumericalMethod = menu('Choose a numerical method:', ...
                       'Euler','Verlet');
					   
%* Set initial position and velocity of pendulum
theta0 = input('Enter initial angle (in degrees): ');
theta = theta0*pi/180;   % Convert angle to radians
omega = 0;               % Set the initial velocity

%* Set the physical constants and other variables
g_over_L = 1;            % The constant g/L
time = 0;                % Initial time
irev = 0;                % Used to count number of reversals
tau = input('Enter time step: ');

%* Take one backward step to start Verlet
accel = -g_over_L*sin(theta);    % Gravitational acceleration
theta_old = theta - omega*tau + 0.5*tau^2*accel;    

%* Loop over desired number of steps with given time step
%    and numerical method
nstep = input('Enter number of time steps: ');
for istep=1:nstep  

  %* Record angle and time for plotting
  t_plot(istep) = time;            
  th_plot(istep) = theta*180/pi;   % Convert angle to degrees
  time = time + tau;
  
  %* Compute new position and velocity using 
  %    Euler or Verlet method
  accel = -g_over_L*sin(theta);    % Gravitational acceleration
  if( NumericalMethod == 1 )
    theta_old = theta;               % Save previous angle
    theta = theta + tau*omega;       % Euler method
    omega = omega + tau*accel; 
  else  
    theta_new = 2*theta - theta_old + tau^2*accel;
    theta_old = theta;			   % Verlet method
    theta = theta_new;  
  end
  
  %* Test if the pendulum has passed through theta = 0;
  %    if yes, use time to estimate period
  if( theta*theta_old < 0 )  % Test position for sign change
    fprintf('Turning point at time t= %f \n',time);
    if( irev == 0 )          % If this is the first change,
      time_old = time;       % just record the time
    else
      period(irev) = 2*(time - time_old);
      time_old = time;
    end
    irev = irev + 1;       % Increment the number of reversals
  end
end

%* Estimate period of oscillation, including error bar
AvePeriod = mean(period);
ErrorBar = std(period)/sqrt(irev);
fprintf('Average period = %g +/- %g\n', AvePeriod,ErrorBar);

%* Graph the oscillations as theta versus time
clf;  figure(gcf);         % Clear and forward figure window
plot(t_plot,th_plot,'+');
xlabel('Time');  ylabel('\theta (degrees)');
