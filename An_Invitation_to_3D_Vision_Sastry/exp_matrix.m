%  akes rotation matrix returns angle-axis representation
function[omega, theta] = exp_matrix(R)
theta = 0;
theta = acos((R(1,1) + R(2,2) + R(3,3)-1)/2);

if theta ~= 0 
omega = 1/(2*sin(theta))*[R(3,2)-R(2,3) R(1,3)-R(3,1) R(2,1)-R(1,2)]';
else
omega = [1 0 0]';  
theta = 0;
%error(' Rotation matrix arbitrary');
end
