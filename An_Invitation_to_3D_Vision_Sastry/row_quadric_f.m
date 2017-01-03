% generates one row of constraints for absolute quadric 
% output constraint of absolute quadric 5 coefficients - symmetric 4x4 matrix
% assumes only the focal lenght is unknown
function [row] = row_quadric_f(p1, p2); 

a = kron(p1', p2);

row(1) = a(1,1) + a(2,2);
row(2) = a(1,4) + a(4,1);
row(3) = a(2,4) + a(4,2);
row(4) = a(3,4) + a(4,3);
row(5) = a(4,4);
row(6) = a(3,3);