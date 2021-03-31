%% Wave1DFriction.m

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

% using an explicit central difference method for the 1D wave equation with friction:
% U_tt + 2*Kappa*U_t = c^2*U_xx
x1=0;
a = 50.; % length of bar in x-direction
x2=a;

T = 20.; % length of time for solution

n = 399;   % no of grid points Xn
m = 1600.;

dx = a/(n+1.); % grid spacing in x direction

dt = T/m;

% frictional coefficient
kappa=0.05  %0.05

t=0.; % initial time = 0

c = 2.; % wave speed 

s = (c^2)*(dt^2)/(dx^2);  % gain parameter 

CourantCondition = c*dt/dx;  % Courant condition 

if CourantCondition > 1.
   fprintf('Courant Condition is > 1 so central difference method is unstable, please reduce time step size to gain stability');
end

% build the A matrix to march finite difference solution forward in time

% check first with n=3 to test that you are getting the right A matrix and b vector:

%s=1  % checkpoint values comment out later!!!
%n=3  % checkpoint values comment out later!!!

Adiag = 2.*(1-s)*ones(n,1);

Asubs = s*ones(n,1);

Asuper = s*ones(n,1);

A = (1./(1.+kappa*dt))*spdiags([Asubs,Adiag,Asuper],[-1 0 1],n,n);

% specify boundary conditions through vector b and by changing any rows in A matrix needed - for Neumann boundary conditions

% we have u(0,t) = 0 = u(a,t)
 
b= zeros(n,1);

% initial conditions for du/dt

% and u_t(x,0) = 0. 

d = zeros(n,1);

e = (1.-kappa*dt)/(1.+kappa*dt)

% set up mesh in x-direction:
x = linspace(x1+dx,x2-dx, n)';
% Set up vector U^0 at time tk=0: U(x,0) = f(x)
% using U(x,0) = plucked string in 2 places: 
 
% U_{tk-1} = U_tk_last
U_tk_last= zeros(n,1);

for i = 1:n
    if x(i) >= 0.1*a
       if x(i) <= 0.2*a
          U_tk_last(i)= 5*(10*x(i)-a);
       elseif x(i) <= 0.3*a
          U_tk_last(i)= 5*(-10*x(i)+3.*a);
       elseif x(i) >= 0.7*a
          if x(i) <= 0.8*a 
             U_tk_last(i)= 5*(10*x(i)-7.*a);
          elseif x(i) <= 0.9*a
             U_tk_last(i)= 5*(-10*x(i)+9.*a);
          end

       end

    end
end

% first we initialise and find U^1 = U_tk at time k=1

t = t+dt;
U_tk = ((1+kappa*dt)/2)*A*U_tk_last + b/2. + d;


figure(1)
plot (x,U_tk_last,'-')
title ('Initial condition for plucked string')
xlabel ('x')
ylabel ('U')

figure(2)
plot (x,U_tk,'-')
title ('Vibrations of string after 1 time step')
xlabel ('x')
ylabel ('U')


% store solution for each time in matrix U(nxm):

U=zeros(n,m);
tvec=zeros(m,1);

U(:,1) = U_tk_last;
tvec(1) = t-dt;
U(:,2) = U_tk;
tvec(2) = t;



% now march solution forward in time using U_tk+1 = A*U_tk + b:

for k = 2:m
    t = t+dt;
    % if boundary conditions vary with time you need to update b here
    U_tk_1 = A*U_tk + b - e*U_tk_last;    

    U(:,k) = U_tk_1;
 
    % for next time step:
    U_tk_last = U_tk;
    U_tk = U_tk_1;
    tvec(k) = t;

end



figure(3)
mesh (x,tvec,U')
title ('String vibrations')
xlabel ('x')
ylabel ('t')
zlabel ('U')


































