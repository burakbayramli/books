function H = rhodtenu(rho)
%
% calcuates the partial derivative matrix of
% the time-derivative of the rotation vector
% rotating ENU coordinates to RPY coordinates
% with respect to inertial rates of the ENU coordinates
%
theta = norm(rho);
if abs(theta) < 1e-5
   H = -eye(3);
else
   u = rho/theta;
   H = -u*u' - (theta*sin(theta))/(2*(1-cos(theta)))*(eye(3) - u*u') + (theta/2)*[0,-u(3),u(2);u(3),0,-u(1);-u(2),u(1),0];
end;
