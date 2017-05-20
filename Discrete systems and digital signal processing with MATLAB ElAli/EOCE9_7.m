function zdot=eoce7non(t,z); % return the state derivatives
	global A;
	zdot=[z(2)-z(1);10+A*cos(t)-z(1)-z(2).^2];

    
    function zdot=eoce7lin(t,z); % return the state derivatives
	global A;
	zdot=[z(2)-z(1);-z(1)-2*2.7016*z(2)+A*cos(t)];

    clf
	global A;
	A=1;
	tspan=[0 20]; % the time span for the simulation
	z0n=[0 0 ];
	[t,z]=ode23('eoce7non',tspan, z0n);
	plot(t,z(:,1),'b*'); % the voltage in the capacitor
	hold on
	plot(t,z(:,2),'b+'); %The current in the inductor
	z0l=[-2.7017 -2.7016];
	[t,z]=ode23('eoce7lin',tspan, z0l);
	plot(t,z(:,1)+2.7016);% for linearized
	plot(t,z(:,2)+2.7016);% for linearized
	title('The linearized and the nonlinear systems for 	z1n=z2n=2.7016 and A = 1');
	gtext('*nonlinear,Vc(t)');
	gtext('+nonlinear, IL(t)')
	xlabel('time in seconds');
