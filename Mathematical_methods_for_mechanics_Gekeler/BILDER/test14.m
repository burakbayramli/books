function Y = test14
X = rand(4);
A = X(1); B = X(2); C = X(3); D = X(4);
E = [A, B; C, D];
F = 1/(D - C*B/A);
INVE = [(1 + C*F*B/A)/A,  - B*F/A; -F*C/A,  F];

AA = E*INVE;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
A = rand(3,3); B = rand(3,3); C = rand(3,3); D = rand(3,3);
E = [A, B; C, D];
F = inv(D - C*inv(A)*B);

INVE = [inv(A)*(eye(3) + B*F*C*inv(A)), - inv(A)*B*F;
        -F*C*inv(A) ,  F];
AA = E*INVE

