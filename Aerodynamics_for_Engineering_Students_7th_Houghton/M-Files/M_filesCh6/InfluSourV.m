function [AN,AT,XC,YC,NHAT,THAT] = InfluSourV(XP,YP,N,NT,NTP1)
% Influence coefficients for source distribution over a 
% symmetric body.
%
NP1 = N + 1;
for J = 1:N
    AN(J,NP1) = 0;
    AT(J,NP1) = pi;
if J==1
  XPL = XP(N);
  YPL = YP(N);
else
  XPL = XP(J-1);
  YPL = YP(J-1);
end
XC(J) = 0.5*(XP(J) + XPL); 
YC(J) = 0.5*(YP(J) + YPL);
S(J) = sqrt( (XP(J) - XPL)^2 + (YP(J) - YPL)^2 ); 
THAT(J,1) = (XP(J) - XPL)/S(J); 
THAT(J,2) = (YP(J) - YPL)/S(J); 
NHAT(J,1) = - THAT(J,2); 
NHAT(J,2) =   THAT(J,1); 
end
%Calculation of the influence coefficients.
for I = 1:N
for  J = 1:N
if I==J
AN(I,J) = pi;
AT(I,J) = 0;
else
DX = XC(I) - XC(J);
DY = YC(I) - YC(J); 
XQ = DX*THAT(J,1) + DY*THAT(J,2); 
YQ = DX*NHAT(J,1) + DY*NHAT(J,2);
VX = -0.5*(  log( (XQ + S(J)/2 )^2 + YQ*YQ )...
    -log( (XQ - S(J)/2 )^2 + YQ*YQ )  );
VY = -( atan2((XQ + S(J)/2 ),YQ) - atan2((XQ - S(J)/2),YQ ));
NTIJ = 0;
NNIJ = 0;
TTIJ = 0;
TNIJ = 0;
for K = 1:2
NTIJ = NHAT(I,K)*THAT(J,K) + NTIJ;
NNIJ = NHAT(I,K)*NHAT(J,K) + NNIJ;
TTIJ = THAT(I,K)*THAT(J,K) + TTIJ;
TNIJ = THAT(I,K)*NHAT(J,K) + TNIJ;
end
AN(I,J) = VX*NTIJ + VY*NNIJ; 
AT(I,J) = VX*TTIJ + VY*TNIJ; 
AN(I,NP1) = AN(I,NP1) + VY*NTIJ - VX*NNIJ; 
AT(I,NP1) = AT(I,NP1) + VY*TTIJ - VX*TNIJ; 
end
end
end
for n = 1:NP1
    AN(NP1,n) = -(AT(NT,n) + AT(NTP1,n));
    AT(NP1,n) = 0;
end
AT(NP1,NP1) = pi;