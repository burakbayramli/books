function [Q] = Euler2D(Q, FinalTime, BC)

% function [Q] = Euler2D(Q,FinalTime,BC);
% Purpose  : Integrate 2D Euler equations using a 3rd order SSP-RK

Globals2D;

% Initialize filter
Filt = CutOffFilter2D(N,0.95);

% compute initial timestep
gamma = 1.4;
dt = EulerDT2D(Q, gamma);
time = 0; tstep=1;

% storage for low storage RK time stepping
rhsQ = 0*Q; resQ = 0*Q;

% filter initial solution
for n=1:4, Q(:,:,n) = Filt*Q(:,:,n); end;

% outer time step loop 
while (time<FinalTime)

  % check to see if we need to adjust for final time step
  if(time+dt>FinalTime)
    dt = FinalTime-time;
  end
  
  for INTRK = 1:5    
    % compute right hand side of compressible Euler equations
    rhsQ  = EulerRHS2D(Q, time, BC);
    
    % filter residual
    for n=1:4, rhsQ(:,:,n) = Filt*rhsQ(:,:,n); end;
    
    % initiate and increment Runge-Kutta residuals
    resQ = rk4a(INTRK)*resQ + dt*rhsQ;  
    
    % update fields
    Q = Q+rk4b(INTRK)*resQ;  
  end;
  
  % Increment time and compute new timestep
  time = time+dt
  dt = EulerDT2D(Q, gamma);

  tstep = tstep+1;
end;
return;
