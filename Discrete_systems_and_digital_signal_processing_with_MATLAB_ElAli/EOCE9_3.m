function zdot=eoce3non(t,z);% return the state derivatives
	global A;
	zdot=[z(2);-z(2)-z(1).^2+A*sin(t)];

    function zdot=eoce3lin(t,z);% return the state derivatives
	global A;
	zdot=[z(2);-z(2)+A*sin(t)];

    
    global A;
	A=0.2;
	clf   % clears the plots
	tspan=[0 20]; % the time span for the simulation
	z0=[0 0];  % initial condition vector
	[t,z]=ode23('eoce3non',tspan, z0);
	plot(t,z(:,1),'g*');
	hold on
	[t,z]=ode23('eoce3lin',tspan, z0);
	plot(t,z(:,1),'b+');
	title('solution of the systems A=0.2');
	gtext('linearized system')
	gtext('nonlinear system')
	xlabel('time in seconds');
