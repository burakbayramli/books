%% fd_c_1_box.m

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

% using the Lax method for the flux conservative problem: U_t = -c U_x
c=1.;
x1=0;
a = 2.*pi; % length of bar in x-direction
x2=a;

T = 5.; % length of time for solution

n = 1023;   % no of grid points Xn

dx = a/(n+1.); % grid spacing in x direction


% dt = dx no dispersion, dt = dx/2 meets Courant stability condition with dispersion, dt=1.001*dx does not meet Courant stability condition

dt = dx;

m = round(T/dt); 

t=0.; % initial time = 0


CourantCondition = c*dt/dx;  % gain parameter 

if CourantCondition > 1. 
   fprintf('Courant Condition is > 1 so Lax method is unstable, please reduce time step size to gain stability');
end

s=c*dt/dx;

% build the A matrix to march finite difference solution forward in time

% check first with n=3 to test that you are getting the right A matrix and b vector:

%s=2  % checkpoint values comment out later!!!
%n=3  % checkpoint values comment out later!!!

Asubs = ((1+s)/2.)*ones(n,1);

Asuper = ((1-s)/2.)*ones(n,1);

A = spdiags([Asubs,Asuper],[-1 1],n,n);

% specify boundary conditions through vector b and by changing any rows in A matrix needed - for Neumann boundary conditions
 
b= zeros(n,1);

% for Dirichlet boundary conditions at x=0: b[1] = (1+s)*U(0,t_k)/2.
% U(x=0, t) = 0.
b(1) = 0.;
% for Dirichlet boundary conditions at x = a: b[n] = (1-s)*U(a,t_k)/2
% U(x=a,t) = 0.
b(n) =  0;
% b % check b is right too!

% set up mesh in x-direction:
x = linspace(x1+dx,x2-dx, n)';
% Set up vector U^0 at time tk=0: U(x,0) = f(x)
% using U(x,0) = 1, if 0.2 < x < 0.4 ; = 0 elsewhere 
 
U_tk= zeros(n,1);

for i = 1:n
    if x(i) >= 0.5
       if x(i) <= 1. 
          U_tk(i) = 1;
       end
    end
end


figure(1)
plot (x,U_tk,'-')
%axis ([0 2 0 1.1])
title ('Initial condition for U')
xlabel ('x')
ylabel ('U')

% store solution for each time in matrix U(nxm):

U=zeros(n,round(m/100));
tvec=zeros(round(m/100),1);

U(:,1) = U_tk;
tvec(1) = t;

% now march solution forward in time using U_tk+1 = A*U_tk + b:

kk=1;

for k = 1:m
    t = t+dt;
    % if boundary conditions vary with time you need to update b here
    U_tk_1 = A*U_tk + b;    

    % for next time step:
    U_tk = U_tk_1;

    if  mod(k,100)==0
        tvec(kk+1) = t;
        U(:,kk+1) = U_tk_1;
        kk = kk+1;
    end
end


figure(2)
waterfall (x,tvec,U')
%axis ([0 1 0 2 0 1.1])
title ('Variation of U with time')
xlabel ('x')
ylabel ('t')
zlabel ('U')


figure(3)
plot (x,U_tk_1)
%axis ([0 2 0 1.1])
title('Final solution at t=5')
xlabel ('x')
ylabel ('U')
































