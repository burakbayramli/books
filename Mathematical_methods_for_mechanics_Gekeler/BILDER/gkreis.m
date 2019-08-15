function DD = gkreis(PHI,THETA);

% PHI   enth„lt   phi-Kordinaten von Anfangs-, Endpunkt
% THETA enth„lt theta-Kordinaten von Anfangs-, Endpunkt
% PHI, THETA Spaltenvektoren

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
%for i = 1:M
%ll = sqrt(DD(:,i)'*DD(:,i))
%end
