
%6.3  onewaycode.m

%From Persson
%This convects a pulse using Upwind, L-F, or L-W.
%Also 3 eps-figures of the results.

% Scheme: Choose one only
%method='upwind';
%method='laxfriedrichs';
method='laxwendroff';

% Grid and initial condition
n=300;
x=(0:n)'/n;
u=double(x>0.2 & x<0.4);

% Discretization in space and time
dx=1/n;
r=1/2;
dt=r*dx;

% Integrate until t=0.4
nt=0.4/dt;

% Main loop
for it=1:nt
  % Take a step
  switch method
   case 'upwind'
    u(2:n)=u(2:n)-r*(u(2:n)-u(1:n-1)); 
   case 'laxfriedrichs'
    u(2:n-1)=(u(3:n)+u(1:n-2))/2-r/2*(u(3:n)-u(1:n-2));
   case 'laxwendroff'
    u(2:n-1)=u(2:n-1)-r/2*(u(3:n)-u(1:n-2))+ ...
             r^2/2*(u(3:n)-2*u(2:n-1)+u(1:n-2));;
  end
  % Plot numerical and exact solution
  uexact=double(x-it*dt>0.2 & x-it*dt<0.4);
  plot(x,u,x,uexact),axis([0,1,-.3,1.3]),grid on,drawnow
end
