function Q = CurvedEuler2D(Q, FinalTime, ExactSolution, ExactSolutionBC, fluxtype)

% function Q = CurvedEuler2D(Q, FinalTime, ExactSolution, ExactSolutionBC, fluxtype)
% Purpose  : Integrate 2D Euler equations using a 4th order low storage RK

Globals2D;

% build cubature information
CubatureOrder = floor(2*(N+1)*3/2); cub = CubatureVolumeMesh2D(CubatureOrder);

% build Gauss node data
NGauss = floor((N+1)*2); gauss = GaussFaceMesh2D(NGauss);

% compute initial timestep
gamma = 1.4;
dt = EulerDT2D(Q, gamma); tstep = 1; time = 0;
rhsQ = 0*Q; resQ = 0*Q;

% outer time step loop 
while (time<FinalTime)
  
  if(time+dt>FinalTime)
    dt = FinalTime-time;
  end
  
  % 3rd order SSP Runge-Kutta
  rhsQ  = CurvedEulerRHS2D(Q, time, ExactSolutionBC, fluxtype);
  Q1 = Q + dt*rhsQ;
  
  rhsQ  = CurvedEulerRHS2D(Q1, time, ExactSolutionBC, fluxtype);
  Q2 = (3*Q + Q1 + dt*rhsQ)/4;
  
  rhsQ  = CurvedEulerRHS2D(Q2, time, ExactSolutionBC, fluxtype);
  Q = (Q + 2*Q2 + 2*dt*rhsQ)/3;

  % Increment time and compute new timestep
  time = time+dt;
  dt = EulerDT2D(Q, gamma);
  
  tstep = tstep+1;
end;
return;
