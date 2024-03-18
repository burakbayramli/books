function R = incr_rotmat(dRoll,dPitch,dYaw)
% small angles only; but orthogonal
E = eye(3);

f  = sqrt(1+dRoll^2);
dR = E;
dR(1,1) = f;
dR(2,3) =-dRoll/f;
dR(3,2) = dRoll/f;
dR      = dR/f;

f  = sqrt(1+dPitch^2);
dP = E;
dP(2,2) = f;
dP(1,3) =-dPitch;
dP(3,1) = dPitch;
dP      = dP/f;

f  = sqrt(1+dYaw^2);
dY = E;
dY(3,3) = f;
dY(1,2) =-dYaw;
dY(2,1) = dYaw;
dY      = dY/f;
R = dR*dP*dY;


