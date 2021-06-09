function [J,f] = JfReservoir(x,z,c)
% JFreservoir  Jacobian and f vector for three reservoir system
%
% Synopsis:  [J,f] = JFreservoir(x,z,c)
%
% Input: x = current guess at solution vector
%        z = vector of elevations of reservoirs and junction, (m)
%            z = [z1; z2; z3; zj]
%        c = constant in Darcy-Weisbach equation, c = f (L/d)(V^2/2/g)(1/A)
%            for each pipe.
%
% Output: J = Jacobian matrix for the system
%         f = right hand side vector for Newton iterations

gamw = 9790;                           %  specific weight of water, (N/m^3) 
q = x(1:3);   pj = x(4);   zj = z(4);  %  extract problem variables from x
f = zeros(4,1);   J = zeros(4,4);      %  Initialize

% ---- equation 1:  sum(q) = 0
f(1) = q(1) + q(2) + q(3);
J(1,1) = 1;  J(1,2) = 1;  J(1,3) = 1;

% ---- equation 2:  head loss in branch 1
f(2) = pj/gamw + sign(q(1))*c(1)*q(1)^2 + zj - z(1);
J(2,1) = sign(q(1))*2*c(1)*q(1);
J(2,4) = 1/gamw;

% --- equation 3:  head loss in branch 2
f(3) = pj/gamw + sign(q(2))*c(2)*q(2)^2 + zj - z(2);
J(3,2) = sign(q(2))*2*c(2)*q(2);
J(3,4) = 1/gamw;

% --- equation 4:  head loss in branch 3
f(4) = pj/gamw + sign(q(3))*c(3)*q(3)^2 + zj - z(3);
J(4,3) = sign(q(3))*2*c(3)*q(3);
J(4,4) = 1/gamw;
