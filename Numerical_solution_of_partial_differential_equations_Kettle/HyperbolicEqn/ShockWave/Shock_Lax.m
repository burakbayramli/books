%% Shock_Lax.m

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

% using the Lax method for Burger's nonlinear equation for inviscid flow: U_t = -U U_x
x1=0;
a = 1.; % length of bar in x-direction
x2=a;

T = 1.; % length of time for solution

n = 999;   % no of grid points Xn

dx = a/(n+1.); % grid spacing in x direction


% dt = dx no dispersion, dt = dx/2 meets Courant stability condition with dispersion, dt=1.001*dx does not meet Courant stability condition

dt = dx/5.;

m = round(T/dt); 

t=0.; % initial time = 0


CourantCondition = dt/dx;  % Courant condition uses c=max(U(x,0)) = 1 for this example 

if CourantCondition > 1. 
   fprintf('Courant Condition is > 1 so Lax method is unstable, please reduce time step size to gain stability');
end

s=dt/dx;

% build the A matrix to march finite difference solution forward in time

% check first with n=3 to test that you are getting the right A matrix and b vector:

%s=2  % checkpoint values comment out later!!!
%n=3  % checkpoint values comment out later!!!

%  in this example the matrix A for the finite difference solution has a constant term, and a varyint term which depends on U(x_j,t_k))

Asubs = (1/2.)*ones(n,1);

Asuper = (1/2.)*ones(n,1);

Aconstant = spdiags([Asubs,Asuper],[-1 1],n,n);

% specify boundary conditions through vector b and by changing any rows in A matrix needed - for Neumann boundary conditions
 
b= zeros(n,1);

% for Dirichlet boundary conditions at x=0: b[1] = (1+s *U(x_1,t_k))*U(0,t_k)/2.
% U(x=0, t) = 0.
b(1) = 0.;
% for Dirichlet boundary conditions at x = a: b[n] = (1-s*U(x_n,t_k))*U(a,t_k)/2
% U(x=a,t) = = U(x_n+1,t_k) = 0.
b(n) =  0;
% b % check b is right too!

% set up mesh in x-direction:
x = linspace(x1+dx,x2-dx, n)';
% Set up vector U^0 at time tk=0: U(x,0) = f(x)
% using U(x,0) = 1, if 0.2 < x < 0.4 ; = 0 elsewhere 
 
U_tk= exp(-10.*((4.*x-1).^2));

figure(1)
plot (x,U_tk,'-')
axis ([0 1 0 1.1])
title ('Initial condition for U')
xlabel ('x')
ylabel ('U')

% store solution for each time in matrix U(nxm):

U=zeros(n,m);
tvec=zeros(m,1);

U(:,1) = U_tk;
tvec(1) = t;

% now march solution forward in time using U_tk+1 = A*U_tk + b:

for k = 1:m
    t = t+dt;
    % if boundary conditions vary with time you need to update b here
    % because A has a varying term we need to calculate that here:

    % padded with zero for Asubs and Asuper because spdiags doesn't use the 0 entries (anything could be entered here)

    Asubs = (1/2.)*s*[U_tk(2:n);0];

    Asuper = -(1/2.)*s*[0; U_tk( 1:n-1)];

    Avary = spdiags([Asubs,Asuper],[-1 1],n,n);

    A = Aconstant+Avary;

    U_tk_1 = A*U_tk + b;    

    U(:,k) = U_tk_1;
 
    % for next time step:
    U_tk = U_tk_1;
    tvec(k) = t;

end


figure(2)
mesh (x,tvec,U')
%axis ([0 1 0 2 0 1.1])
title ('Variation of U with time using Lax Method')
xlabel ('x')
ylabel ('t')
zlabel ('U')


figure(3)
plot (x,U_tk_1)
axis ([0 1 0 1.1])
title('Final solution at t=1')
xlabel ('x')
ylabel ('U')
































