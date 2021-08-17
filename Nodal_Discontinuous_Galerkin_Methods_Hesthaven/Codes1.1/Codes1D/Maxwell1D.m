function [E,H] = Maxwell1D(E,H,eps,mu,FinalTime);

% function [E,H] = Maxwell1D(E,H,eps,mu,FinalTime)
% Purpose  : Integrate 1D Maxwell's until FinalTime starting with conditions (E(t=0),H(t=0))
%            and materials (eps,mu).

Globals1D;
time = 0;

% Runge-Kutta residual storage  
resE = zeros(Np,K); resH = zeros(Np,K); 

% compute time step size
xmin = min(abs(x(1,:)-x(2,:)));
CFL=1.0;  dt = CFL*xmin;
Nsteps = ceil(FinalTime/dt); dt = FinalTime/Nsteps

% outer time step loop 
for tstep=1:Nsteps
   for INTRK = 1:5
      [rhsE, rhsH] = MaxwellRHS1D(E,H,eps,mu);
      
      resE = rk4a(INTRK)*resE + dt*rhsE;
      resH = rk4a(INTRK)*resH + dt*rhsH;
      
      E = E+rk4b(INTRK)*resE;
      H = H+rk4b(INTRK)*resH;
   end 
   % Increment time
   time = time+dt;
end
return
