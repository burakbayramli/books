function V = coriolis(coriolis_para,theta,theta_dot)
theta1 = theta(1);
theta2 = theta(2);

theta1_dot = theta_dot(1);
theta2_dot = theta_dot(2);

p1 = coriolis_para.p1;
p2 = coriolis_para.p2;

V = [-p1*sin(theta2)*(2*(theta1_dot*theta2_dot) +theta2_dot^2);p2*sin(theta2)*theta1_dot^2];