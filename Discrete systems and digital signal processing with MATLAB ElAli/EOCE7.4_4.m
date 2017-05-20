	clf % to clear the graph area
	global A
	global B
	t0=0;	
	tf=10;
	z0=[0;0]; % vector of initial conditions
	A=[-6 -2;4 0];
	B=[2;0];
	[t,z]=ode23('eoce4',t0,tf,z0); % a call to the ode23
	y1=2.5*z(:,2);
	plot(t,y1,'go');
	xlabel('Time in seconds')
	A=[0 1;-8 -6];
	B=[0;20];
	[t,z]=ode23('eoce3',t0,tf,z0); % a call to the ode23
	y1=z(:,1);
	hold on
	plot(t,y1,'b*');
	title('The output y(t) for the MATLAB and the derived 	model')
