function test01
% Check of Voigt's representation
U = rand(1,11);
ux = U(1); uy = U(2); uz = U(3);
vx = U(4); vy = U(5); vz = U(6);
wx = U(7); wy = U(8); wz = U(9);
E = U(10); nu = abs(U(11));
EE = [ux, (uy+vx)/2, (uz+wx)/2;
     (uy+vx)/2, uy, (vz + wy)/2;
      (uz+wx)/2,(vz + wy)/2, uz];
      
      trace = EE(1,1)+EE(2,2)+EE(3,3);
SS = (E/(1+nu))*(EE +  + nu*trace*eye(3)/(1-2*nu));
EE = EE(:); SS = SS(:);
RESULT1 = EE.'*SS
      
C = zeros(6,6);
C(1,1) = 1-nu; C(1,2) = nu; C(1,3) = nu;
C(2,1) = nu;   C(2,2) = 1-nu; C(2,3) = nu;
C(3,1) = nu; C(3,2) = nu; C(3,3) = 1- nu;
C(4,4) = (1-2*nu)/2; C(5,5) = C(4,4); C(6,6) = C(4,4);
C = E*C/((1+nu)*(1-2*nu));
EV = [ux;uy;uz; uy+vx; uz+wx;,vz+wy]   ;
SV = C*EV;
RESULT2 = EV.'*SV