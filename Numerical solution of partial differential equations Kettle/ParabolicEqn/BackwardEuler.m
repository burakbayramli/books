%% BackwardEuler.m

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

% using an implicit backward Euler method for the heat equation U_t = beta*U_xx
x1=0;
a = 1.; % length of bar in x-direction
x2=a;

T = 12000.; % length of time for solution

n = 39;   % no of grid points Xn
m = 100.;

dx = a/(n+1.); % grid spacing in x direction

dt = T/m;

t=0.; % initial time = 0

beta = 1.e-5; % thermal diffusivity constant

s = beta*dt/(dx^2);  % gain parameter 

if s > 0.5
   fprintf('gain parameter > 0.5 - implicit backward Euler method is still stable');
end

% build the A matrix to march finite difference solution forward in time

% check first with n=3 to test that you are getting the right A matrix and b vector:

%s=1  % checkpoint values comment out later!!!
%n=3  % checkpoint values comment out later!!!

% because we have a Neumann boundary condition at x=a we are solving for U_1 to U_(n+1)

Adiag = (1+2*s)*ones(n+1,1);

Asubs = -s*ones(n+1,1);

Asuper = -s*ones(n+1,1);

A = spdiags([Asubs,Adiag,Asuper],[-1 0 1],n+1,n+1);

% specify boundary conditions through vector b and by changing any rows in A matrix needed - for Neumann boundary conditions
 
% for Neumann boundary conditions at x = a: (approximating du/dx with leapfrog in spatial derivative)
A(n+1,n) = -2.*s;

b= zeros(n+1,1);

% for Dirichlet boundary conditions at x=0: b[1] = s*g1(t_k)
% U(x=0, t) = 1.
b(1) = s*1.;
% for Neumann boundary conditions at x = a: b[n] = s*dx*g2(t_k)
% U_x(x=a,t) = 2.
b(n+1) =  4.*s*dx;
% b % check b is right too!

% set up mesh in x-direction:
x = linspace(x1+dx,x2, n+1)';

% Set up vector U^0 at time tk=0: U(x,0) = f(x)
% using U(x,0) = 2*x + sin(2*pi*x) + 1
 
U_tk = 2.*x + sin(2*pi*x) + 1.;

figure(1)
plot (x,U_tk,'-')
title ('Initial condition for Temperature distribution')
xlabel ('x')
ylabel ('U')

% store solution for each time in matrix U((n+1) x m):

U=zeros(n+1,m);
tvec=zeros(m,1);

U(:,1) = U_tk;
tvec(1) = t;

% now march solution forward in time using U_tk+1 = inverse(A)*(U_tk + b):

for k = 1:m
    t = t+dt;
    % if boundary conditions vary with time you need to update b here
    % with implicit method we solve a matrix equation at each step:
    c = U_tk + b;
    U_tk_1 = A\c;  
    % this is very time consuming later we will discuss faster ways to solve this problem using iterative methods

    U(:,k) = U_tk_1;
 
    % for next time step:
    U_tk = U_tk_1;
    tvec(k) = t;

end



figure(2)
mesh (tvec,x,U)
title ('Variation of Temperature distribution with time using Backward Euler method')
xlabel ('t')
ylabel ('x')
zlabel ('U')


































