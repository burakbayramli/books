clf	% clearing latest plots
	A=1; % the amplitude of the step
	t=0:0.01:10;	% generating points in time
	% generating a row of length t of magnitude A
	unitstep=A*ones(1,length(t));
	plot(t,unitstep);
	title('MATLAB simulated unit step signal');
	xlabel('t in seconds');
	axis([0 10 0 1.2]);	% fix the x and y axis
