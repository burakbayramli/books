
function cooling_all
%
% thermal evolution of mantle following a simplified version of 
% Schubert et al (1980)
% 
% no heat flux from core
% 
% see: Schubert et al., Mantle convection, Cambridge Press, p.590ff
%
% height of mantle
L = 2871e3;
% diffusivity
kappa = 1e-6;
%
% all times are expressed as the characteristic, diffusion time
%
tc = L^2/kappa;
% gravitational acceleration
grav=10;
% thermal expansivity
alpha=3e-5;
% heat production over hear capacity
H0overcp = 4.317e-14;
% radioactive decay timescale
lambda = 1.42e-17 *tc;
% critical Rayleigh number
Rac=1100;
% conductivity
k=4.18;
% reference density
rho=3400;
% reference viscosity prefactor, eta = eta0*exp(E0/T)
eta0=1.65e2*rho;
% surface area of earth over (mass of mantle times heat capacity)
AoverMcp = 1.377e-13;
%
sec_year = 364.25 * 24 * 60 * 60;
% max integration time
tmax = (4.6e9 * sec_year)/tc;
% Ga per tc
gat=tc/(1e9*sec_year);
%
%
% first constant in evolution law
f1 = H0overcp  * tc;
%
% second constant is written as two here, f2=f2a * f2b^beta
%
f2a=AoverMcp*k/L*tc;
f2b=(alpha*grav*L^3*rho)/(kappa*eta0*Rac);
%
% other parameters
%
E0=70000;
beta=0.2;
% solve ODE
options=odeset;
[t1,T1] = ...
    ode45(@dTdt,[0;tmax],2000,options,f1,lambda,f2a,f2b,beta,E0);
[t2,T2] = ...
    ode45(@dTdt,[0;tmax],3000,options,f1,lambda,f2a,f2b,beta,E0);
[t3,T3] = ...
    ode45(@dTdt,[0;tmax],4000,options,f1,lambda,f2a,f2b,beta,E0);

%
% plot
%
figure(1),clf;
subplot(2,1,1);
% temperature
plot(t1*gat,T1,t2*gat,T2,t3*gat,T3);
axis([0 4.6 1500 3500]);
legend('T_0 = 2000 K','T_0 = 3000 K','T_0 = 4000 K');
xlabel('time [Ga]');ylabel('temperature [K]');
subplot(2,1,2);
% viscosity
semilogy(t1*gat,eta(T1,eta0,E0),... 
    t2*gat,eta(T2,eta0,E0),t3*gat,eta(T3,eta0,E0));
axis([0 4.6 1e14 1e23 ]);
xlabel('time [Ga]');
ylabel('viscosity [Pas]');


%
% time derivative of temperature
%
function dTdt = dTdt(t,T,f1,lambda,f2a,f2b,beta,E0)
dTdt = f1 * exp(-lambda*t) - ... 
    f2a*(f2b^beta)*(T-273.)^(1+beta)*exp(-beta*E0/T);

%
% viscosity law
%
function eta = eta(T,eta0,E0)
eta = eta0 * exp(E0./T);



