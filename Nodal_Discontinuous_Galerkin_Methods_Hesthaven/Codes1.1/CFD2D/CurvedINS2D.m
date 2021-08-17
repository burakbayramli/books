function [Ux, Uy, PR, time] = CurvedINS2D(...
	  Ux, Uy, PR, FinalTime, nu, simtype, ExactSolution, BCfunction);

% function [Ux, Uy, PR, time] = CurvedINS2D(...
%           Ux, Uy, PR, FinalTime, nu, simtype, ExactSolution, BCfunction);
% Purpose: integrate the incompressible Navier-Stokes equations to FinalTime

Globals2D;

% build cubature nodes for all elements
Nint = ceil(3*N/2);
CubatureOrder = 2*(Nint+1); cub = CubatureVolumeMesh2D(CubatureOrder);

% build Gauss node data for all element faces
NGauss = (Nint+1); gauss = GaussFaceMesh2D(NGauss);

% time step parameter
dt = min(dtscale2D())/((N+1)^2); 
Nsteps = ceil(FinalTime/dt); dt = FinalTime/Nsteps;

% dual splitting scheme coefficients
g0 = 1.0; a0 = 1; a1 = 0; b0 = 1; b1 = 0; 

% Build pressure matrix and boundary forcing (IPDG)
[PRperm, PRsystemC, PRsystemCT, refrhsbcPR] = CurvedINSPressureSetUp2D(dt, nu, BCfunction);

% Build viscous matrix and boundary forcing (IPDG)
[VELperm, VELsystemC, VELsystemCT, refrhsbcUx, refrhsbcUy] = ...
    CurvedINSViscousSetUp2D(dt, nu, g0, BCfunction);

% Form inhomogeneous boundary term for rhs data 
[refbcUx,refbcUy,refbcPR, refbcdUndt] = ...
    feval(BCfunction, Fx, Fy, nx, ny, mapI, mapO, mapW, mapC, 0, nu);  

% storage for history of fields and nonlinear terms
zer = zeros(Np, K); Uxold = Ux; NUx = zer; Uyold = Uy; NUy = zer; 

% storage for pressure Neumann boundary data
dpdn = zeros(Nfp*Nfaces,K);

% initial time
time = 0; tstep = 1;
for tstep=1:Nsteps

  % take 2 time steps at first order
  if(tstep==2)
    g0 = 1.5; a0 = 2; a1 = -.5; b0 = 2; b1 = -1; 
    
    % Rebuild pressure and viscous matrixes for new g0
    [PRperm, PRsystemC, PRsystemCT, refrhsbcPR] = ...
	CurvedINSPressureSetUp2D(dt, nu, BCfunction);
    
    [VELperm, VELsystemC, VELsystemCT, refrhsbcUx, refrhsbcUy] = ...
	CurvedINSViscousSetUp2D(dt, nu, g0, BCfunction);

  end

  % temporal scaling factor for bc data
  tfac = 1; tfac1 = 1; tpfac = 1; tpfac1 = 1;  tpfac2 = 1;
  if(strfind(simtype, 'VolkerCylinder'))
     tfac = sin(pi*time/8); tfac1 = sin(pi*(time+dt)/8); 
     tpfac = (pi/8)*cos(pi*time/8); tpfac1 = (pi/8)*cos(pi*(time)/8); 
     tpfac2 = (pi/8)*cos(pi*(time)/8);
  end
  if(strfind(simtype, 'PearsonVortex'))
    tfac = exp(-nu*4*pi^2*time); tfac1 = exp(-nu*4*pi^2*(time+dt)); 
    tpfac = -4*nu*pi^2*exp(-nu*4*pi^2*time); 
    tpfac1 = exp(-nu*8*pi^2*(time)); tpfac2 = exp(-nu*8*pi^2*(time+dt)); 
  end

  bcUx = tfac*refbcUx; rhsbcUx = tfac1*refrhsbcUx; 
  bcUy = tfac*refbcUy; rhsbcUy = tfac1*refrhsbcUy; 
  bcPR = tpfac1*refbcPR; rhsbcPR = tpfac2*refrhsbcPR; 
  bcdUndt = tpfac*refbcdUndt;

  % script to compute nonlinear terms NUx, NUy
  INSAdvection2D; 

  % script to compute pressure PR and intermediate UxTT, UyTT
  INSPressure2D;

  % script to compute viscous solves and update velocity
  CurvedINSViscous2D;

  % Increment time 
  time = tstep*dt;

  if((tstep==1) | ~mod(tstep,100) | (tstep==Nsteps))  
     [exUx, exUy, exPR] = feval(ExactSolution, x, y, time, nu);
     
     % render data
     INSRender2D(time,N,Ux-exUx,Uy-exUy,PR-exPR);
  end
  if(strfind(simtype, 'VolkerCylinder'))
     % compute drag and lift coefficients, and pressure drop
    [Cd,Cl,dP] = INSLiftDrag2D(Ux, Uy, PR, 0.05, nu, time, tstep, Nsteps);
  end

end
return
