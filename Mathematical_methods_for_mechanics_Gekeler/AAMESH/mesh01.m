function [p1,e1,el1] = mesh01(p,e,el)
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% uniform mesh refinement for edges, triangular and
% quadrangular meshs

[K,L] = size(p);
[M,N] = size(el);
if M == 2
   p1  = zeros(K,N);
   el1 = zeros(M,2*N);
   R   = [1:2];
   for I = 1:N
      L1 = L+1;
      el1(:,2*(I-1)+R) = [el(1,I),       L1;
                               L1,  el(2,I)];
      J = el(1:2,I);
      p1(:,I)= (p(1:K,J(1))+p(1:K,J(2)))/2;
      L = L+1;
   end
end
if M == 3
   p1  = zeros(K,3*N);
   el1 = zeros(M,4*N);
   R   = [1:4]; S = [1:3];
   for I = 1:N
      L1 = L+1; L2 = L+2; L3 = L+3;
      el1(:,4*(I-1)+R) = [el(1,I),      L1,      L3,  L1;
                               L1, el(2,I),      L2,  L2;
                               L3,      L2, el(3,I),  L3];
      J = el(1:3,I);
      p1(:,3*(I-1)+S)=[ (p(1:K,J(1))+p(1:K,J(2)))/2, ...
                        (p(1:K,J(2))+p(1:K,J(3)))/2, ...
                        (p(1:K,J(3))+p(1:K,J(1)))/2];
      L = L+3;
   end
end
if M == 4
   p1 = zeros(K,4*N);
   el1 = zeros(M,4*N);
   R   = [1:4]; S = [1:5];
   for I = 1:N
      L1 = L+1; L2 = L+2; L3 = L+3; L4 = L+4; L5 = L+5;
      el1(:,4*(I-1)+R) = [el(1,I),      L1,      L5,     L4;
                               L1, el(2,I),      L2,     L5;
                               L5,      L2, el(3,I),     L3;
                               L4,      L5,      L3, el(4,I)];
      J = el(1:4,I);
      p1(:,5*(I-1)+S)=[(p(1:K,J(1)) + p(1:K,J(2)))/2, ...
                        (p(1:K,J(2)) + p(1:K,J(3)))/2, ...
                        (p(1:K,J(3)) + p(1:K,J(4)))/2, ...
                        (p(1:K,J(4)) + p(1:K,J(1)))/2, ...
                        (p(1:K,J(1)) + p(1:K,J(2))...
                        + p(1:K,J(3))+ p(1:K,J(4)))/4];

      L = L+5;
   end
end
p1       = [p, p1];
% -- doppelte Knoten eliminieren ----------
[p1,el1] = mesh04(p1,el1);
% -- Rand verfeinern ----------------------
AUX = [];
for I = 1:size(e,2)
   X  = p1(1,e(1:2,I)); Y = p1(2,e(1:2,I));
   ZWP = [(X(1) + X(2))/2;(Y(1)+Y(2))/2];
   DIFF = p1 - ZWP*ones(1,size(p1,2));
   NORMDIFF = sqrt(DIFF(1,:).*DIFF(1,:) + DIFF(2,:).*DIFF(2,:));
   J = find(NORMDIFF == min(NORMDIFF));
   AUX1 = [[e(1,I);J;e(3:5,I)],[J;e(2,I);e(3:5,I)]];
   AUX  = [AUX,AUX1];
end
e1 = AUX;
