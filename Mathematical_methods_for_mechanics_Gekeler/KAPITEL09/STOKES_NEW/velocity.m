function V = velocity(p,e,t,Z);
% Computation of velocity

X = p(1,:); Y = p(2,:); M = size(t,2); VN = zeros(2,M);
for I = 1:size(t,2)
   K  = t(:,I);
   X21 = X(K(2))-X(K(1)); X31 = X(K(3))-X(K(1)); X32 = X(K(3))-X(K(2));
   Y21 = Y(K(2))-Y(K(1)); Y31 = Y(K(3))-Y(K(1)); Y32 = Y(K(3))-Y(K(2));
   DEL(I) = (X21*Y31 - X31*Y21);
   VN(1,I) = (X32*Z(K(1))-X31*Z(K(2))+X21*Z(K(3)))/DEL(I);
   VN(2,I) =-(-Y32*Z(K(1))+Y31*Z(K(2))-Y21*Z(K(3)))/DEL(I);
end
V = VN;
%V = pdeprtni(p,t,VN);
