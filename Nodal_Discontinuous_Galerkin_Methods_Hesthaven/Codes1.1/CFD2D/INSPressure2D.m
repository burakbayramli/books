% Purpose: script which performs the pressure step for the incompressible Navier-Stokes solver

% compute divergence of UxT and UyT
DivUT = Div2D(UxT, UyT);

% dp/dn = -n.(u.grad u + curl curl u)
[tmp,tmp,CurlU] = Curl2D(Ux,Uy,[]);
[dCurlUdx,dCurlUdy] = Grad2D(CurlU);

res1 =  -NUx - nu*dCurlUdy; res2 = -NUy + nu*dCurlUdx;

% save old and compute new dp/dn
dpdnold = dpdn; dpdn = zeros(Nfp*Nfaces, K);

% decide which nodes to apply Neumann boundary conditions for pressure
nbcmapD = [mapI; mapW; mapC]; vbcmapD = [vmapI; vmapW; vmapC]; 
dpdn(nbcmapD) = (nx(nbcmapD).*res1(vbcmapD) + ny(nbcmapD).*res2(vbcmapD)); 
dpdn = dpdn - bcdUndt;

% Evaluate right hand side term for Poisson equation for pressure
PRrhs = MassMatrix*(J.*(-DivUT*g0/dt) + LIFT*(sJ.*(b0*dpdn + b1*dpdnold)));

% Add Dirichlet boundary condition forcing
PRrhs(:) = PRrhs(:) + rhsbcPR(:);

% Pressure Solve, -laplace PR = +(div UT)/dt + LIFT*dpdn on boundaries
PRrhs(:) = PRrhs(PRperm); tmp =  PRsystemCT\PRrhs(:); PR(PRperm) = PRsystemC\tmp;

% compute  (U~~,V~~) = (U~,V~) - dt*grad PR
[dPRdx,dPRdy] = Grad2D(PR);

% increment (Ux~,Uy~) to (Ux~~,Uy~~)
UxTT = UxT - dt*(dPRdx)/g0; UyTT = UyT - dt*(dPRdy)/g0;
