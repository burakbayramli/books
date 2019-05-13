%Coriolis matrix for a two link manipulator
%Written by Thrishantha Nanayakkara
% For the book: Intelligent Systems with an Introduction to System of
% Systems Control
function V = coriolis(coriolis_para,theta1,theta2,theta1_dot,theta2_dot)

p1 = coriolis_para.p1;
p2 = coriolis_para.p2;

V = [-p1*sin(theta2)*(2*(theta1_dot*theta2_dot)^2+theta2_dot^2);p2*sin(theta2)*theta1_dot^2];