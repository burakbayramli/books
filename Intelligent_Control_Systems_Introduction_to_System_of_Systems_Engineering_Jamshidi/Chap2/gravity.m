%Gravity matrix for a two link manipulator
%Written by Thrishantha Nanayakkara
% For the book: Intelligent Systems with an Introduction to System of
% Systems Control
function G = gravity(gravity_para,theta1,theta2)
p1 = gravity_para.p1;
p2 = gravity_para.p2;
p3 = gravity_para.p3;


G = [p1*sin(theta1 + theta2)+(p2*sin(theta1));p3*sin(theta1+theta2)];