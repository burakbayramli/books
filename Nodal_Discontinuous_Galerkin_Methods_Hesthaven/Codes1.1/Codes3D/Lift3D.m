function [LIFT] = Lift3D(N,R,S,T)

% function [LIFT] = Lift3D(N, r, s, t)
% Purpose  : Compute 3D surface to volume lift operator used in DG formulation

Globals3D;

Emat = zeros(Np, Nfaces*Nfp);

for face=1:Nfaces
  % process face
  if(face==1); faceR = R(Fmask(:,1)); faceS = S(Fmask(:,1)); end;
  if(face==2); faceR = R(Fmask(:,2)); faceS = T(Fmask(:,2)); end;
  if(face==3); faceR = S(Fmask(:,3)); faceS = T(Fmask(:,3)); end;
  if(face==4); faceR = S(Fmask(:,4)); faceS = T(Fmask(:,4)); end;
  
  VFace = Vandermonde2D(N, faceR, faceS);
  massFace = inv(VFace*VFace');

  idr = Fmask(:,face); idc = (face-1)*Nfp+1:face*Nfp;

  Emat(idr, idc) = Emat(idr, idc)+ massFace;
end

% inv(mass matrix)*\I_n (L_i,L_j)_{edge_n}
LIFT = V*(V'*Emat);
return
