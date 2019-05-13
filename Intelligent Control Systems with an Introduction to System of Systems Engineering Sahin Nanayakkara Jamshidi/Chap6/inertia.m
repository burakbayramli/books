
function M = inertia(iner_para,theta)
theta1 = theta(1);
theta2 = theta(2);

p1 = iner_para.p1;
p2 = iner_para.p2;
p3 = iner_para.p3;
p4 = iner_para.p4;
p5 = iner_para.p5;

M = [(p1+p2*cos(theta2)) (p3+p4*cos(theta2));(p3+p4*cos(theta2)) p5];