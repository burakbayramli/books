function test04
% Kinematic Euler equations
% Check of Euler rotation matrix

clc
%syms u1 u2 u3
%DM1 = [cos(u1), -sin(u1), 0; sin(u1), cos(u1), 0; 0,0, 1];
%DM2 = [1 0 0; 0, cos(u2), -sin(u2); 0, sin(u2), cos(u2)];
%DM3 = [cos(u3), -sin(u3), 0; sin(u3), cos(u3), 0; 0,0, 1];
%DDM = DM1*DM2*DM3;



syms om1 om2 om3 a b c t
u1 = a*t; u2 = b*t; u3 = c*t; 
C = [0, -om3, om2; om3, 0, -om1; -om2, om1, 0];
D1 = [cos(u3)*cos(u1)-sin(u3)*cos(u2)*sin(u1);
      cos(u3)*sin(u1)+sin(u3)*cos(u2)*cos(u1);
      sin(u3)*sin(u2)]; 
      
D2 = [-sin(u3)*cos(u1)-cos(u3)*cos(u2)*sin(u1);
      -sin(u3)*sin(u1)+cos(u3)*cos(u2)*cos(u1);
      cos(u3)*sin(u2)]; 
      
D3 = [ sin(u2)*sin(u1);
      -sin(u2)*cos(u1);
      cos(u2)];
D = [D1,D2,D3];

%DIFF = DDM - D;
%DIFF = simplify(DIFF) 
D1T = diff(D1,'t');
D1T = simplify(D1T)
D2T = diff(D2,'t');
D2T = simplify(D2T)
D3T = diff(D3,'t');
D3T = simplify(D3T)

DDT = [D1T,D2T,D3T];
PROD = DDT*D.';
PROD = simplify(PROD)

