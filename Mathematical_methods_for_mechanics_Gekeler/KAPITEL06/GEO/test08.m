function test08
%  Check of kinematic Euler equations numerically
% by calculating OMEGA_F by omega_E 
clc

RANDOM = rand(3,1);
phi = RANDOM(1); thet = RANDOM(2); psi = RANDOM(3);

DC = [0, cos(phi),  sin(thet)*sin(phi);
      0, sin(phi), -sin(thet)*cos(phi);
      1,        0,  cos(thet)];
DE = [sin(thet)*sin(psi), cos(psi), 0;
      sin(thet)*cos(psi), -sin(psi), 0;
      cos(thet),      0,             1];
DB1 = [cos(psi)*cos(phi), - sin(psi)*cos(phi),   sin(thet)*sin(phi);
       cos(psi)*sin(phi), - sin(psi)*sin(phi), - sin(thet)*cos(phi);
       sin(psi)*sin(thet),  cos(psi)*sin(thet),  cos(thet)];

DB2 = [- sin(psi)*cos(thet)*sin(phi), - cos(psi)*cos(thet)*sin(phi), 0;
        sin(psi)*cos(thet)*cos(phi),    cos(psi)*cos(thet)*cos(phi), 0;
                                  0,                              0, 0];
DB1 = DB1';
DB2 = DB2';
DB = DB1 + DB2;
DF = DB*DC;
DIFF = DE - DF

