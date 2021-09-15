% batch_kinetics_dynamics_ex.m
function x_dot = batch_kinetics_dynamics_ex(t,x,k1);

cA = x(1);  cB = x(2);
r1 = k1*cA*cB;
x_dot = zeros(3,1);
x_dot(1) = -r1;
x_dot(2) = -r1;
x_dot(3) = r1;

return;

