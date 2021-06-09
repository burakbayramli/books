clf
	A=[-1 1;1 -2];
	B=[0;104];
	C=[1 0];
	D=[0];
	[y z t]=step(A,B,C,D);
	T1= 104*0.1708*(exp(-2.618*t)-1)-104*1.1707*(exp(-0.382*t)-1);
	plot(t,y(:,1),'*',t,T1,'-');
	title('the step response for eoce9')
	ylabel('Temperature within capacitance C1')
	xlabel(' time in seconds')
	gtext('* MATLAB simulation, - analytical result')
