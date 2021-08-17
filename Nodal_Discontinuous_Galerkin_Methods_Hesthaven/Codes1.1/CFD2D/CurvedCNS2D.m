function Q = CurvedCNS2D(Q, FinalTime, ExactSolution, ExactSolutionBC)

% function Q = CurvedCNS2D(Q, FinalTime, ExactSolution, ExactSolutionBC)
% Purpose  : Integrate 2D compressible Navier-Stokes using a 4th order low storage RK

Globals2D;

% build cubature information
CubatureOrder = 3*(N+1); cub = CubatureVolumeMesh2D(CubatureOrder);

% build Gauss node data
NGauss = ceil(3*(N+1)/2); gauss = GaussFaceMesh2D(NGauss);

% compute initial timestep
gamma = 1.4; mu = 1e-2;
dt = 0.25*CurvedCNSdt2D(Q, gamma, mu); tstep = 1; time = 0;

rhsQ = 0*Q; resQ = 0*Q;

% outer time step loop 
while (time<FinalTime)

  if(time+dt>FinalTime)
    dt = FinalTime-time;
  end
  
  for INTRK = 1:5 
    RKtime = time+dt*rk4c(INTRK);
    
    % compute right hand side of compressible Navier-Stokes equations
    rhsQ  = CurvedCNSRHS2D(Q, mu, RKtime, ExactSolutionBC, 'LF');
    
    % initiate and increment Runge-Kutta residuals
    resQ = rk4a(INTRK)*resQ + dt*rhsQ;  
    
    % update fields
    Q = Q+rk4b(INTRK)*resQ;  
  end;
  
  % Increment time and compute new timestep
  time = time+dt
  
  dt = 0.25*CurvedCNSdt2D(Q, gamma, mu); 
  
  tstep = tstep+1;
end;
return;
