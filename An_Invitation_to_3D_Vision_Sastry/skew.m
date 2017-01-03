% Return associated skew matrix of a given 3D vector



function S_hat = skew(S)

S_hat = zeros(3,3);

S_hat(1,2) = - S(3); S_hat(1,3) = S(2);   S_hat(2,3) = - S(1);

S_hat(2,1) = S(3);   S_hat(3,1) = - S(2); S_hat(3,2) = S(1);

