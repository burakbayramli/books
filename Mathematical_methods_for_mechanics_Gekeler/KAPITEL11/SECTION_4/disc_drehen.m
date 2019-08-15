function [YN,AINB,AONB] = disc_drehen(PHI,X,Y)
cs   = cos(PHI); ss = sin(PHI);
DREH = [cs, -ss; ss, cs];
YN  = DREH*Y;
[AINB,AONB] = inpolygon(X(1,:),X(2,:),YN(1,:),YN(2,:));
