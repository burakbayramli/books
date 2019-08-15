function A = koffizienten(p,Z)
N = size(p,2)-1;
A = zeros(5,N);
for I = 1:N
   L = abs(p(I+1) - p(I));
   a = Z(1,I); b = Z(2,I);
   c = 3*(Z(1,I+1) - Z(1,I))/L^2 - (2*Z(2,I) + Z(2,I+1))/L;
   d = 2*(Z(1,I) - Z(1,I+1))/L^3 + (Z(2,I+1) + Z(2,I))/L^2;
   A(:,I) = [a;b;c;d;L];
end
