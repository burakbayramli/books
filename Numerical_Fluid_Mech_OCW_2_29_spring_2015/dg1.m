clear all, clc, clf, close all
pkg load symbolic;
syms x
%create nodal basis
%Set order of basis function
%N >=2
N = 3;
%Create basis
if N==3
  theta = [1/2*x^2-1/2*x;
	   1- x^2;
	   1/2*x^2+1/2*x];
else
  xi = linspace(-1,1,N);
  for i=1:N
    theta(i)=sym('1');
    for j=1:N
      if j~=i
	theta(i) = ...
	theta(i)*(x-xi(j))/(xi(i)-xi(j));
      end
    end
  end
end

%Create mass matrix
for i = 1:N
  for j = 1:N
    %Create integrand
    intgr = int(theta(i)*theta(j));
    %Integrate
    M(i,j) =...
    subs(intgr,1)-subs(intgr,-1);
  end
end
%create convection matrix
for i = 1:N
  for j = 1:N
    %Create integrand
    intgr = ...
    int(diff(theta(i))*theta(j));
    %Integrate
    K(i,j) = ...
    subs(intgr,1)-subs(intgr,-1);
  end
end

%% Initialize u
Nx = 20;
dx = 1./Nx;
%Multiply Jacobian through mass matrix.
%Note computationl domain has length=2,
actual domain length = dx
M=M*dx/2;
%Create "mesh"
x = zeros(N,Nx);
for i = 1:N
  x(i,:) =...
  dx/(N-1)*(i-1):dx:1-dx/(N-1)*(N-i);
end
%Initialize u vector
u = exp(-(x-.5).^2/.1^2);
%Set timestep and velocity
dt=0.002;
c=1;
%Periodic domain
ids = [Nx,1:Nx-1];

%Integrate over time
for i = 1:10/dt
  u0=u;
  %Integrate with 4th order RK
  for irk=4:-1:1
    %Always use upwind flux
    r = c*K*u;
    %upwinding
    r(end,:) = r(end,:) - c*u(end,:);
    %upwinding
    r(1,:) = r(1,:) + c*u(end,ids);
    %RK scheme
    u = u0 + dt/irk*(M\r);
  end
  %Plot solution
  if ~mod(i,10)
    plot(x,u,'b');
    drawnow;
  end
end

