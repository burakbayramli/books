%Inertia matrix for a two link manipulator
%Written by Thrishantha Nanayakkara
% For the book: Intelligent Systems with an Introduction to System of
% Systems Control
function M = inertia(iner_para,theta1,theta2)

p1 = iner_para.p1;
p2 = iner_para.p2;
p3 = iner_para.p3;
p4 = iner_para.p4;
p5 = iner_para.p5;

M = [(p1+p2*cos(theta2)) (p3+p4*cos(theta2));(p3+p4*cos(theta2)) p5];