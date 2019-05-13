function G = gravity(gravity_para,theta)
theta1 = theta(1);
theta2 = theta(2);

p1 = gravity_para.p1;
p2 = gravity_para.p2;
p3 = gravity_para.p3;


G = [p1*sin(theta1 + theta2)+(p2*sin(theta1));p3*sin(theta1+theta2)];