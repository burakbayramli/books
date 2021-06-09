clf 	% clearing latest plots
	A=1; 	% the slope of the ramp
	t=0:0.5:10;	% generating points in time
	ramp=A*t;	% generating the ramp signal
	plot(t,ramp);
	title('MATLAB simulated ramp signal');
	xlabel('t in seconds');
