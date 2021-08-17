function Q = EulerShock2D(Q, FinalTime, ExactSolution, ExactSolutionBC, fluxtype)

% function Q = EulerShock2D(Q,FinalTime, ExactSolution, ExactSolutionBC, fluxtype)
% Purpose  : Integrate 2D Euler equations using a 2nd order SSP Runge-Kutta time integrator

Globals2D;

% build cubature information
CubatureOrder = floor(2*(N+1)*3/2); cub = CubatureVolumeMesh2D(CubatureOrder);

% build Gauss node data
NGauss = floor((N+1)*2); gauss = GaussFaceMesh2D(NGauss);

% compute initial timestep
gamma = 1.4; dt = EulerDT2D(Q, gamma); tstep = 1; time = 0;

% limit initial condition
Q = EulerLimiter2D(Q, ExactSolutionBC, time);

% set up rendering
figure(1); 
[triplot, xplot, yplot,uplot, interpplot] = PlotField2D(N, x, y, Q(:,:,1)); drawnow; pause(.02);

rhsQ = 0*Q; resQ = 0*Q;

% outer time step loop 
while (time<FinalTime)
  
  if(time+dt>FinalTime)
    dt = FinalTime-time;
  end

  oldQ = Q;

  % 2nd order SSP Runge-Kutta
  rhsQ  = CurvedEulerRHS2D(Q, time, ExactSolutionBC, fluxtype);
  Q1 = Q + dt*rhsQ;
  Q1 = EulerLimiter2D(Q1, ExactSolutionBC, time);
  
  rhsQ  = CurvedEulerRHS2D(Q1, time, ExactSolutionBC, fluxtype);
  Q = (Q + Q1 + dt*rhsQ)/2;
  Q = EulerLimiter2D(Q, ExactSolutionBC, time);
  
  % Increment time and compute new timestep
  time = time+dt
  dt = EulerDT2D(Q, gamma)

  resid(tstep) = sqrt(sum(sum(sum((Q-oldQ).^2)))/(4*K*Np));

  % Render every 25 time steps
  if(~mod(tstep,25))
    timeanddt = [tstep, time, dt]
    
    subplot(2,2,1); trisurf(triplot, xplot, yplot, interpplot*Q(:,:,1)); 
    shading interp; view(2); title('density');

    subplot(2,2,2); trisurf(triplot, xplot, yplot, interpplot*Q(:,:,2)); 
    shading interp; view(2); title('x-momentum');

    subplot(2,2,3); trisurf(triplot, xplot, yplot, interpplot*Q(:,:,3)); 
    shading interp; view(2); title('y-momentum');

    subplot(2,2,4); trisurf(triplot, xplot, yplot, interpplot*Q(:,:,4)); 
    shading interp; view(2); title('Energy');
    
    drawnow; pause(.05);
  end

  tstep = tstep+1;
end 
return;
