function DD = gkreis(PHI,THETA);
% draws shortest connection between two points on unit sphere 
% PHI   contains phi coordinates of initial and terminal point
% THETA contains theta coordinates of initial and termiinal point
% PHI, THETA column vectors
N = 16;
A = [cos(PHI(1))*cos(THETA(1));
     sin(PHI(1))*cos(THETA(1));
     sin(THETA(1))];
B = [cos(PHI(2))*cos(THETA(2));
     sin(PHI(2))*cos(THETA(2));
     sin(THETA(2))];
ALPHA = acos(A'*B);
C = [A(2)*B(3) - A(3)*B(2);
     A(3)*B(1) - A(1)*B(3);
     A(1)*B(2) - A(2)*B(1)];
BETA = ALPHA*[0,1:N]/N;
M = length(BETA);
DD = zeros(3,M);
al = sqrt(A'*A);
for I = 1:M
   DD(:,I) = dmatrix(BETA(I),C)*A;
end
