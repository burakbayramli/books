clf
	A=[0 1;-1 0];
	B=[1;0];
	C=[1 0];
	D=[0];
	[y z t]=impulse(A,B,C,D);%return the states and the outputs
	theta_analytical=cos(t);
	omega_analytical=-sin(t);
	plot(t,z(:,1),'*',t,theta_analytical,'o',t,z(:,2), t,omega_analytical,'o')
	gtext('* displacement');
	gtext('o velocity');
	title('analytical and MATLAB simulation for eoce8')
	xlabel('time in seconds')
	ylabel('Amplitudes')
