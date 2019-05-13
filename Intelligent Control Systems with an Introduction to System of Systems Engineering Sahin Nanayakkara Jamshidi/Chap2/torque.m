%Model based controller a two link manipulator
%Written by Thrishantha Nanayakkara
% For the book: Intelligent Systems with an Introduction to System of
% Systems Control
function tau = torque(theta1_ddot,theta2_ddot,M,V,G)

tau = M*[theta1_ddot;theta2_ddot] + V + G;