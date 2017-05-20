clf 	% clearing latest plots
	A=1; % the amplitude of the sinusoidal signal
	f=1; % the frequency of the signal
	phi=0; % the phase of the signal in radians
	% generating the sinusoidal signal
	t=0:0.01:10;	% generating points in time
	sinusoidal=A*cos(2*pi*t+phi);		
	plot(t,sinusoidal);
	title('MATLAB simulated sinusoidal signal');
	xlabel('t in seconds');
