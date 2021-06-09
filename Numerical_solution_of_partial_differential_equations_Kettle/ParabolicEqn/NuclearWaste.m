%% NuclearWaste.m 

%% Author: Louise Olsen-Kettle (email: l.kettle1@uq.edu.au, @DrOlsen-Kettle)
%% Code to supplement lecture notes by Louise Olsen-Kettle:
%% Numerical solution of Partial Differential Equations (PDEs)
%% Webpage: http://espace.library.uq.edu.au/view/UQ:239427
%% ISBN: 978-1-74272-149-1

% using an implicit Backward Euler method for the radioactive decay of a nuclear rod for 0 < r < 100cm, 0 < t < 100years
% solving T_t = kappa*(T_rr + T_r/r) + S(r,t)


r1=0;
rc=100.; % r_c = 100cm 
r2=rc;

T = 100.; % length of time for solution

n = 99;   % no of grid points r_j 
m = 1000.;

dr = rc/(n+1.); % grid spacing in r direction

dt = T/m;



t=0.; % initial time = 0

kappa = 2.e7; % thermal diffusivity constant

s = kappa*dt/(dr^2);  % gain parameter 

% build the A matrix to march finite difference solution forward in time

% check first with n=3 to test that you are getting the right A matrix and b vector:

%s=1  % checkpoint values comment out later!!!
%n=3  % checkpoint values comment out later!!!

% we are solving for T_1 to T_n

A=zeros(n,n);

for i = 1:n
    for j = 1:n

        if i==j
           A(i,j) = 1. + 2.*s;
        elseif i==j-1
           A(i,j) = -s - s/(2.*i);
        elseif i==j+1
           A(i,j) = -s + s/(2.*i);
        else
           A(i,j) = 0.;
        end
    end
end

% specify boundary conditions through vector b and by changing any rows in A matrix needed - for Neumann boundary conditions

% for Neumann boundary conditions at r=0: dT(0,t)/dr = 0 is approximated by T_0^k = T_1^k

A(1,1) = 1+s+s/2;

b= zeros(n,1);

% for Dirichlet boundary conditions at r=rc: b[n] = (-s-s/(2*n))*300. 
% U(r=rc, t) = 300.
b(n) = -(-s-s/(2*n))*300.;

%b % check b is right too!

% set up mesh in r-direction:
r = linspace(r1+dr,r2-dr, n)';

% Set up vector T^0 at time tk=0: T(r,0) = 300K 
 
T_tk = 300.*ones(n,1);

figure(1)
plot (r,T_tk,'-')
title ('Initial condition for Temperature distribution')
xlabel ('r')
ylabel ('Temperature')

% store solution for each time in matrix Temp(n x m):

Temp=zeros(n,m);
tvec=zeros(m,1);

Temp(:,1) = T_tk;
tvec(1) = t;

% now march solution forward in time using U_tk+1 = inverse(A)*(U_tk + b):

% define source term due to radioactive decay of nuclear rod:
% source vector is only nonzero for r < a=25cm

source_vector = zeros(n,1);
a_source = 25.;   % a = 25cm radius of rod is 25cm
Trod = 1.;  % initial temperature change due to nuclear rod is 1K
tau_0 = 100;  % half-life of rod is 100 years

for i = 1:n
    if r(i) < a_source
       source_vector(i) = 1;
    end
end

temp_solutions=zeros(4,1);
isolution = 1;

for k = 1:m
    t = t+dt;
    % if boundary conditions vary with time you need to update b here
    % with implicit method we solve a matrix equation at each step:
 
    % source term depends on time
    source = (Trod*exp(-t/tau_0)/(a_source^2))*source_vector;

    c = T_tk + b + kappa*dt*source;
    T_tk_1 = A\c;  
    % this is very time consuming later we will discuss faster ways to solve this problem using iterative methods

    Temp(:,k) = T_tk_1;
 
    % for next time step:
    T_tk = T_tk_1;
    tvec(k) = t;

    % record temperature at t = 1,10,50 and 100 years
    if k==10
       temp_solutions(isolution) = t
       isolution = isolution+1;
       Temp1year = T_tk_1;
    elseif k==100
       temp_solutions(isolution) = t
       isolution = isolution+1;
       Temp10year = T_tk_1;
    elseif k==500
       temp_solutions(isolution) = t
       isolution = isolution+1;
       Temp50year = T_tk_1;
    elseif k==1000
       temp_solutions(isolution) = t
       isolution = isolution+1;
       Temp100year = T_tk_1;
    end

end



figure(2)
mesh (tvec,r,Temp)
title ('Variation of Temperature distribution with time using Backward Euler method')
xlabel ('t')
ylabel ('r')
zlabel ('Temperature')


figure(3)

plot(r,Temp1year,r,Temp10year,r,Temp50year,r,Temp100year)

legend ('Temp after 1 year', 'Temp after 10 years', 'Temp after 50 years', 'Temp after 100 years')

xlabel ('r')
ylabel ('Temperature')






























