function [Hx,Hy,Ez,time] = MaxwellPNonCon2D(pinfo, Hx, Hy, Ez, FinalTime)

% function [Hx,Hy,Ez] = MaxwellPNonCon2D(pinfo, Hx, Hy, Ez, FinalTime)
% Purpose  : Integrate TM-mode Maxwell's until FinalTime starting with initial conditions Hx,Hy,Ez       

Globals2D;
time = 0;

% Runge-Kutta residual storage  
resHx = zeros(size(Hx)); resHy = resHx; resEz = resHx;

% compute time step size (taking into account variable polynomial order)
dt = 100;
for N=1:length(pinfo)
  dt = min(dt, 2./( N^2*max(pinfo(N).Fscale(:)/2)));
end

% outer time step loop 
tstep = 1;
while (time<FinalTime)
  
   if(time+dt>FinalTime), dt = FinalTime-time; end

   for INTRK = 1:5    
      % compute right hand side of TM-mode Maxwell's equations
      [rhsHx, rhsHy, rhsEz] = MaxwellPNonConRHS2D(pinfo, Hx,Hy,Ez);

      % initiate and increment Runge-Kutta residuals
      resHx = rk4a(INTRK)*resHx + dt*rhsHx;  
      resHy = rk4a(INTRK)*resHy + dt*rhsHy; 
      resEz = rk4a(INTRK)*resEz + dt*rhsEz; 
      
      % update fields
      Hx = Hx+rk4b(INTRK)*resHx;  
      Hy = Hy+rk4b(INTRK)*resHy;  
      Ez = Ez+rk4b(INTRK)*resEz;        
   end;

   % Increment time
   time = time+dt; tstep = tstep+1;
end 
return
