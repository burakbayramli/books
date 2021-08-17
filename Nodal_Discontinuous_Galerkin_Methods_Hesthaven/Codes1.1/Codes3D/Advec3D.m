function [u] = Advec3D(u, FinalTime)

% function [u] = Advec3D(u, FinalTime)
% Purpose  : Integrate 3D advection until FinalTime starting with initial condition, u

Globals3D;
time = 0;

% Runge-Kutta residual storage  
resu = zeros(Np,K); 

% compute time step size
dt = .125/max( ((N+1)^2)*.5*Fscale(:));

% outer time step loop 
tstep = 1;
while (time<FinalTime)

  if(time+dt>FinalTime), dt = FinalTime-time;  end;

  % low storage RK
  for INTRK = 1:5
    timelocal = time + rk4c(INTRK)*dt;
    rhsu = AdvecRHS3D(u,timelocal);
    resu = rk4a(INTRK)*resu + dt*rhsu;
    u = u+rk4b(INTRK)*resu;
  end;

  % Increment time
  time = time+dt; tstep = tstep+1;
end;
return
