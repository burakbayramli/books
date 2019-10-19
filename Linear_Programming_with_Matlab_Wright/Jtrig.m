function Jval = Jtrig(z)
% evaluates the 3 x 3 Jacobian of the example function
Jval = [2*z(1) 2*z(2) 0; sin(z(1)) 0 1 ; 0 -cos(z(2)) 1];

