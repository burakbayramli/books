%% Shock_mol.m

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

% using the method of lines to solve Burger's nonlinear equation for inviscid flow: U_t = -U U_x
global s b n

x1=0;
a = 1.; % length in x-direction
x2=a;

T = 0.1; % length of time for solution

n = 999;   % no of grid points Xn

dx = a/(n+1.); % grid spacing in x direction

s = 1./(dx*2);  

% specify boundary conditions through vector b and by changing any rows in A matrix needed - for Neumann boundary conditions
 
b= zeros(n,1);

% for Dirichlet boundary conditions at x=0: b[1] = s*U(x_1,t_k)*U(x_0,t_k)
% U(x=0, t) = *U(x_0,t_k) = 0.
b(1) = 0.;
% for Dirchlet boundary conditions at x = a: b[n] = -s*U(x_n,t_k)*U(x_n+1,t_k)
% U(x=a,t) = U(x_n+1,t_k) = 0.
b(n) =  0.;
% b % check b is right too!

% set up mesh in x-direction:
x = linspace(x1+dx,x2-dx, n)';
% Set up vector U^0 at time tk=0: U(x,0) = f(x)
 
U0 =  exp(-10.*((4.*x-1).^2));

figure(1)
plot (x,U0,'-')
title ('Initial condition for U')
xlabel ('x')
ylabel ('U')

[t,U] = ode45('Uprime2',[0,T], U0);


figure(2)
mesh (t,x,U')
title ('Variation of U with time using Method of Lines')
xlabel ('t')
ylabel ('x')
zlabel ('U')

last_time_count = max(size(U))
if last_time_count == n
   last_time_count = min(size(U))
end

figure(3)
plot (x,U(last_time_count,:),'-')
title ('Solution at t=0.1')
xlabel ('x')
ylabel ('U')

last_time_count = round(last_time_count/2)

figure(4)
plot (x,U(last_time_count,:),'-')
title ('Solution at t=0.05')
xlabel ('x')
ylabel ('U')






























