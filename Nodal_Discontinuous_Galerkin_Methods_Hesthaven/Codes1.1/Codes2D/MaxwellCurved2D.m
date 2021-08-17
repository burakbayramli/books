function [Hx,Hy,Ez,time] = MaxwellCurved2D(Hx, Hy, Ez, FinalTime)

% function [Hx,Hy,Ez] = MaxwellCurved2D(Hx, Hy, Ez, FinalTime)
% Purpose  : Integrate TM-mode Maxwell's until FinalTime starting with initial conditions Hx,Hy,Ez

Globals2D;
time = 0;

% Runge-Kutta residual storage  
resHx = zeros(Np,K); resHy = zeros(Np,K); resEz = zeros(Np,K); 

% compute time step size
rLGL = JacobiGQ(0,0,N); rmin = abs(rLGL(1)-rLGL(2));
dtscale = dtscale2D; dt = min(dtscale)*rmin*2/3;

cinfo = BuildCurvedOPS2D(3*N);

% outer time step loop 
tstep = 1;
while (time<FinalTime)
  
  if(time+dt>FinalTime), dt = FinalTime-time; end

   for INTRK = 1:5    
      % compute right hand side of TM-mode Maxwell's equations
      [rhsHx, rhsHy, rhsEz] = MaxwellCurvedRHS2D(cinfo, Hx,Hy,Ez);

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

   if(~mod(tstep, 5))
     PlotField2D(2*N, x, y, Ez); 
     hold on; ha = trimesh(EToV, VX, VY); set(ha, 'Color', 'black'); hold off 
     axis([min(x(:)) max(x(:)) min(y(:)) max(y(:)) -1 1]); drawnow; pause(.02);
   end
end
return
