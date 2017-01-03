% computes rotation matrix using Rodriguez formula 
function [rot_matrix] = rot_matrix(omega,theta)
if nargin == 1
 theta = norm(omega);
 omega = omega/norm(omega);
end;
omega_hat = [0 -omega(3) omega(2);
             omega(3) 0  -omega(1);
             -omega(2) omega(1) 0 ];
norm_omega = norm(omega);
if (norm(omega) ~= 0)
rot_matrix = diag([1,1,1])+(omega_hat./norm_omega).* sin(norm_omega*theta) ...
            + ((omega_hat^2)./norm_omega^2) .* (1 - cos(norm_omega*theta));
else
rot_matrix = diag([1 1 1]);
end;
