function zdot=eoce1(t,z)
global A
global B
zdot=A*z+B


clf % to clear the graph area
global A 
global B
t0=0;
tf=10;
z0=[1;0]; % vector of initial conditions
A=[0 1;-4 -2];
B=[0;1];
[t,z]=ode23('eoce1',t0,tf,z0); % a call to the ode23
y1=z(:,1);
plot(t,y1);
xlabel('Time in seconds')
ylabel('The output y(t)')
