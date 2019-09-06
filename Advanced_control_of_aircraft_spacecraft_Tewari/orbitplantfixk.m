% Program 'orbitplantfixk.m' for generating the closed-loop equations
% with a constant gain, planar orbit regulator for an
% elliptical nominal orbit.
% (c) 2009 Ashish Tewari
% Requires the codes 'kepler.m' and 'nominalorbit.m'
function xdot=orbitplantfixk(t,x)
global f9;
mu=398600.4; %Gravitational constant of planet (km^3/s^2)
[r, rdot, h]=nominalorbit(t); % Nominal state variables
% Following is the constant regulator gain matrix computed
% at t=0 for the nominal plant:
k =[1.001249565511754e-4 1.413452700199927e-2 7.079776644590912e-8
2.838185231637881e-6 6.892456255064159e-4 1.004503657414909e-6];
u=-k*[x(1,1)-r; x(2,1)-rdot; x(3,1)-h]; %Control input (km/s^2)
U=norm(u); % magnitude of control input (f_\mathrmT/m) (km/s^2)
alpha=180*atan(u(1,1)/u(2,1))/pi; %Direction of control input (deg.)
fprintf(f9, '\t%1.5e\t%1.5e\t%1.5e\n', t, U, alpha );
% Equations of motion for the perturbed trajectory:
xdot(1,1)=x(2,1); % Radial rate (km/s)
xdot(2,1)=u(1,1)-mu/x(1,1)^2+x(3,1)^2/x(1,1)^3;%Radial accln(km/s^2)
xdot(3,1)=x(1,1)*u(2,1); % Rate of angular momentum (km^2/s^2)
