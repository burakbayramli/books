clf
	A=[0 1;-40 -12];
	B=[1;0];
	C=[1 0];
	D=[0];
	[z y t]=impulse(A,B,C,D);
	hold on
	y_analytical=exp(-6*t).*cos(2*t)+3*exp(-6*t).*sin(2*t);
	plot(t,z(:,1),'*',t,y_analytical,'o')% z(:,1)means z1
	gtext('* analytical, o simulation');
	title('analytical and MATLAB simulation for eoce7')
	xlabel('time in seconds')
	ylabel('Amplitudes')
