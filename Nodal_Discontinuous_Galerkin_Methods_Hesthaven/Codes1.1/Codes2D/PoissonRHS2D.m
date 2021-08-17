function [rhsu,Mu] = PoissonRHS2D(u)

% function [rhsu] = PoissonRHS2D(u)
% Purpose  : Evaluate RHS flux in 2D Heat/Poisson equation using a stabilized internal penalty flux

Globals2D;

% Define field differences at faces and impose Dirichlet BCs
du = zeros(Nfp*Nfaces,K); du(:)=u(vmapM)-u(vmapP); du(mapD) = 2*u(vmapD);

% Compute qx and qy, define differences and impose Neumann BC's
[dudx,dudy] = Grad2D(u);

% Compute DG gradient with central fluxes
fluxxu = nx.*du/2.0; qx = dudx - LIFT*(Fscale.*fluxxu); 
fluxyu = ny.*du/2.0; qy = dudy - LIFT*(Fscale.*fluxyu);

% Compute minimum height of elements either side of each edge
hmin = min(2*J(vmapP)./sJ(mapP), 2*J(vmapM)./sJ(mapM));
tau = reshape(Np./hmin, Nfp*Nfaces, K); 

% Evaluate jumps in components of q at element interfaces
dqx=zeros(Nfp*Nfaces,K); dqx(:)=qx(vmapM)-qx(vmapP); dqx(mapN) = 2*qx(vmapN);
dqy=zeros(Nfp*Nfaces,K); dqy(:)=qy(vmapM)-qy(vmapP); dqy(mapN) = 2*qy(vmapN);

% Evaluate flux function
fluxq = (nx.*dqx + ny.*dqy + tau.*du)/2;

% Compute right hand side
divq = Div2D(qx,qy);

% compute right hand side residual
rhsu = J.*((invV'*invV)*(divq - LIFT*(Fscale.*fluxq)));
Mu = J.*((invV'*invV)*u);
return;
