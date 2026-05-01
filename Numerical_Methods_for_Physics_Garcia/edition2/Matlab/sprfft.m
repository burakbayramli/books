% sprfft - Program to compute the power spectrum of a  
% coupled mass-spring system.
clear; help sprfft;  % Clear memory and print header

%* Set parameters for the system (initial positions, etc.).
x = input('Enter initial displacement [x1 x2 x3]: ');  
v = [0 0 0];       % Masses are initially at rest
state = [x v];     % Positions and velocities; used by rk4
tau = input('Enter timestep: ');  
k_over_m = 1;      % Ratio of spring const. over mass

%* Loop over the desired number of time steps.
time = 0;          % Set initial time
nstep = 256;       % Number of steps in the main loop
nprint = nstep/8;  % Number of steps between printing progress
for istep=1:nstep  %%% MAIN LOOP %%%

  %* Use Runge-Kutta to find new displacements of the masses.
  state = rk4(state,time,tau,'sprrk',k_over_m);  
  time = time + tau;    
  
  %* Record the positions for graphing and to compute spectra.
  xplot(istep,1:3) = state(1:3);   % Record positions
  tplot(istep) = time;
  if( rem(istep,nprint) < 1 )
    fprintf('Finished %g out of %g steps\n',istep,nstep);
  end
end

%* Graph the displacements of the three masses.
figure(1); clf;  % Clear figure 1 window and bring forward
ipr = 1:nprint:nstep;  % Used to graph limited number of symbols
plot(tplot(ipr),xplot(ipr,1),'o',tplot(ipr),xplot(ipr,2),'+',...
     tplot(ipr),xplot(ipr,3),'*',...
     tplot,xplot(:,1),'-',tplot,xplot(:,2),'-.',...
     tplot,xplot(:,3),'--');
legend('Mass #1  ','Mass #2  ','Mass #3  ');
title('Displacement of masses (relative to rest positions)');
xlabel('Time'); ylabel('Displacement');
drawnow;

%* Calculate the power spectrum of the time series for mass #1
f(1:nstep) = (0:(nstep-1))/(tau*nstep);      % Frequency
x1 = xplot(:,1);              % Displacement of mass 1
x1fft = fft(x1);              % Fourier transform of displacement
spect = abs(x1fft).^2;        % Power spectrum of displacement

%* Apply the Hanning window to the time series and calculate
%  the resulting power spectrum
window = 0.5*(1-cos(2*pi*((1:nstep)-1)/nstep)); % Hanning window
x1w = x1 .* window';          % Windowed time series
x1wfft = fft(x1w);            % Fourier transf. (windowed data)
spectw = abs(x1wfft).^2;      % Power spectrum (windowed data)

%* Graph the power spectra for original and windowed data
figure(2); clf;  % Clear figure 2 window and bring forward
semilogy(f(1:(nstep/2)),spect(1:(nstep/2)),'-',...
         f(1:(nstep/2)),spectw(1:(nstep/2)),'--');
title('Power spectrum (dashed is windowed data)');
xlabel('Frequency'); ylabel('Power');
