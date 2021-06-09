function zdot=eoce6non(t,z); % return the state derivatives
	zdot=[1-sqrt(z(1)-z(2));sqrt(z(1)-z(2))-sqrt(z(2))];

    function zdot=eoce6lin(t,z); % return the state derivatives
	zdot=[-0.5*z(1)+0.5*z(2);0.5*z(1)-1*z(2)];

    
    clf
	z1n=2;
	tspan=[0 50]; % the time span for the simulation
	z0=[2.5 1.5];  % initial condition vector
	[t,z]=ode23('eoce6non',tspan, z0);
	plot(t,z(:,1),'g*');% height in the input tank
	hold on
	z0=[0.5 0.5];
	[t,z]=ode23('eoce6lin',tspan, z0);
	plot(t,z(:,1)+z1n,'b+');% z1(t)=deltaz1(t)+z1n
	title('The systems in eoce6 with initial conditions…
 	[2.5 	1.5]');
	gtext('linearized system')
	gtext('nonlinear system')
	xlabel('time in seconds');
