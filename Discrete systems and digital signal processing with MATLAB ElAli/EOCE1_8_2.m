clf 	% clearing latest plots	
	A=1; % the amplitude of the impulse
	t=0:0.5:10;% generating points in time
	%We generate a row of zeros of length t, magnitude A
	unitimpulse=A*zeros(1,length(t));
	unitimpulse(1)=A;	% creating the impulse
	stem(t,unitimpulse);	% show plot as 'o'
	title('MATLAB simulated unit impulse signal');
	xlabel('t in seconds');
	axis([0 10 0 1.2]);		% fix the x and y axis
