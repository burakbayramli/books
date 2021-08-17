function [rhsHx, rhsHy, rhsHz, rhsEx, rhsEy, rhsEz] = MaxwellRHS3D(Hx,Hy,Hz,Ex,Ey,Ez)

% function [rhsHx, rhsHy, rhsHz, rhsEx, rhsEy, rhsEz] = MaxwellRHS3D(Hx,Hy,Hz,Ex,Ey,Ez)
% Purpose  : Evaluate RHS flux in 3D Maxwell equations

Globals3D;

% storage for field differences at faces
dHx = zeros(Nfp*Nfaces,K); dHy = dHx; dHz = dHx; 
dEx = zeros(Nfp*Nfaces,K); dEy = dEx; dEz = dEx; 

% form field differences at faces
dHx(:)  = Hx(vmapP)-Hx(vmapM);  dEx(:)  = Ex(vmapP)-Ex(vmapM);	
dHy(:)  = Hy(vmapP)-Hy(vmapM); 	dEy(:)  = Ey(vmapP)-Ey(vmapM);	
dHz(:)  = Hz(vmapP)-Hz(vmapM);  dEz(:)  = Ez(vmapP)-Ez(vmapM);  

% make boundary conditions all reflective (Ez+ = -Ez-)
dHx(mapB) = 0;  dEx(mapB) = -2*Ex(vmapB); 
dHy(mapB) = 0;  dEy(mapB) = -2*Ey(vmapB); 
dHz(mapB) = 0;  dEz(mapB) = -2*Ez(vmapB);

alpha=1; % => full upwinding

ndotdH = nx.*dHx + ny.*dHy + nz.*dHz;
ndotdE = nx.*dEx + ny.*dEy + nz.*dEz;

fluxHx = -ny.*dEz + nz.*dEy + alpha*(dHx - ndotdH.*nx); 
fluxHy = -nz.*dEx + nx.*dEz + alpha*(dHy - ndotdH.*ny); 
fluxHz = -nx.*dEy + ny.*dEx + alpha*(dHz - ndotdH.*nz); 

fluxEx =  ny.*dHz - nz.*dHy + alpha*(dEx - ndotdE.*nx); 
fluxEy =  nz.*dHx - nx.*dHz + alpha*(dEy - ndotdE.*ny); 
fluxEz =  nx.*dHy - ny.*dHx + alpha*(dEz - ndotdE.*nz); 

% evaluate local spatial derivatives
[curlHx,curlHy,curlHz] = Curl3D(Hx,Hy,Hz);
[curlEx,curlEy,curlEz] = Curl3D(Ex,Ey,Ez);

% calculate Maxwell's right hand side
rhsHx = -curlEx + LIFT*(Fscale.*fluxHx/2);
rhsHy = -curlEy + LIFT*(Fscale.*fluxHy/2);
rhsHz = -curlEz + LIFT*(Fscale.*fluxHz/2);

rhsEx =  curlHx + LIFT*(Fscale.*fluxEx/2);
rhsEy =  curlHy + LIFT*(Fscale.*fluxEy/2);
rhsEz =  curlHz + LIFT*(Fscale.*fluxEz/2);
return;
