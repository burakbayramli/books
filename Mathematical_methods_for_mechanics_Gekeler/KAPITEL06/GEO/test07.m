function test07
%  Check of kinematic Euler equations
% function does not work with psi instead pssi
clc
syms phi thet pssi phid thetd psid 
DDZ3 = [cos(pssi)*sin(thet)*psid + sin(pssi)*cos(thet)*thetd;
        -sin(pssi)*sin(thet)*psid + cos(pssi)*cos(thet)*thetd;
        -sin(thet)*thetd];

DT1  = [cos(pssi)*cos(phi)-sin(pssi)*cos(thet)*sin(phi);
        -sin(pssi)*cos(phi) - cos(pssi)*cos(thet)*sin(phi);
        sin(thet)*sin(phi)];
DT2  = [cos(pssi)*sin(phi)+sin(pssi)*cos(thet)*cos(phi);
        -sin(pssi)*sin(phi)+cos(pssi)*cos(thet)*cos(phi);
        - sin(thet)*cos(phi)];

omga11 = DDZ3(1)*DT2(1);
omga12 = DDZ3(2)*DT2(2);
omga13 = DDZ3(3)*DT2(3);
omga11 = simplify(omga11);
omga12 = simplify(omga12);
omga13 = simplify(omga13);
omga1  = omga11 + omga12 + omga13;
omga1  = simplify(omga1)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
omga21 = DDZ3(1)*DT1(1);
omga22 = DDZ3(2)*DT1(2);
omga23 = DDZ3(3)*DT1(3);
omga21 = simplify(omga21);
omga22 = simplify(omga22);
omga23 = simplify(omga23);
omga2  = omga21 + omga22 + omga23;
omga2  = - simplify(omga2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

DDZ2A = [-sin(pssi)*sin(phi)*psid+cos(pssi)*cos(phi)*phid;
         -cos(pssi)*sin(phi)*psid - sin(pssi)*cos(phi)*phid;
         -cos(thet)*cos(phi)*thetd+sin(thet)*sin(phi)*phid];
DDZ2B = [cos(pssi)*cos(thet)*cos(phi)*psid-sin(pssi)*sin(thet)*cos(phi)*thetd-sin(pssi)*cos(thet)*sin(phi)*phid;
         -sin(pssi)*cos(thet)*cos(phi)*psid-cos(pssi)*sin(thet)*cos(phi)*thetd-cos(pssi)*cos(thet)*sin(phi)*phid;
         0];
DDZ2 = DDZ2A + DDZ2B;
DDZ2 = simplify(DDZ2);
omga31 = DDZ2(1)*DT1(1);
omga31 = simplify(omga31);
omga32 = DDZ2(2)*DT1(2);
omga32 = simplify(omga32);
omga33 = DDZ2(3)*DT1(3);
omga33 = simplify(omga33);
omga3  = omga31 + omga32 + omga33;
omga3  = simplify(omga3)

DC = [0, cos(phi),  sin(thet)*sin(phi);
      0, sin(phi), -sin(thet)*cos(phi);
      1,        0,  cos(thet)];
DB1 = [cos(pssi)*cos(phi), - sin(pssi)*cos(phi),   sin(thet)*sin(phi);
       cos(pssi)*sin(phi), - sin(pssi)*sin(phi), - sin(thet)*cos(phi);
       sin(pssi)*sin(thet),  cos(pssi)*sin(thet),  cos(thet)];

DB2 = [- sin(pssi)*cos(thet)*sin(phi), - cos(pssi)*cos(thet)*sin(phi), 0;
        sin(pssi)*cos(thet)*cos(phi),    cos(pssi)*cos(thet)*cos(phi), 0;
                                  0,                              0, 0];
DB1 = DB1';
DB2 = DB2';
DB = DB1 + DB2;
DE11 = real(DB(1,:)*DC(:,1));
DE11 = simplify(DE11)
DE12 = DB(1,:)*DC(:,2);
DE12 = simplify(DE12)
DE13 = real(DB(1,:)*DC(:,3));
DE13 = simplify(DE13)

%DE = DB*DC;
%DE = simplify(DE)
omga1 = cos(phi)*thetd + sin(thet)*sin(phi)*psid;
omga1_2 = omga1*omga1;
omga2 = sin(phi)*thetd  -sin(thet)*cos(phi)*psid;
omga2_2 = omga2*omga2;
sum = omga1_2 + omga2_2;
sum = simplify(sum)

