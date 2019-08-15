function [X,RES] = fig0501aux(P,Q)
% Common roots of x1 + x^TPx = 0; x2 + x^TQx = 0; x = [x1;x2]
% P = [p11,p12;p12,p22]; Q = [q11,q12;q12,q22]; 
% x1 notequal 0; y = x2/x1;
p11 = P(1,1); p12 = P(1,2); p22 = P(2,2);
q11 = Q(1,1); q12 = Q(1,2); q22 = Q(2,2);
% f(y) = p22*y^3 + (2*p12 - q22)*y^2 + (p11 - 2q12)*y - q11

C = [p22, 2*p12-q22, p11-2*q12, -q11];
AUX = roots(C);
J = find(imag(AUX) == 0);
Z = []; X = []; RES = [];
if ~isempty(J)
   Z = AUX(J);
   L = length(Z); X = zeros(L,2);
   for K = 1:L
      y = Z(K);
      X(K,1) = -1/(p11 + 2*p12*y + p22*y^2); 
      X(K,2) = y*X(K,1);
   end
   for K = 1:L 
       Y = X(K,:).'; 
       RES = [RES, Y + [Y.'*P*Y;Y.'*Q*Y]]; % Check solutions
   end 
end
