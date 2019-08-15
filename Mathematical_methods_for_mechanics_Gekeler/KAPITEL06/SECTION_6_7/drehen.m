function [XN,YN,ZN] = drehen(X,Y,Z,A,phi)
% -- Drehmatix --------------------------
a   = A/norm(A);
C   = [0, -a(3), a(2); a(3), 0, -a(1); -a(2), a(1), 0];
DD  = cos(phi)*eye(3) + (1 - cos(phi))*a*a' + sin(phi)*C;
XN  = 0*X; YN = 0*Y; ZN = 0*Z;
for I = 1:size(X,1)
   AA      = DD*[X(I,:);Y(I,:);Z(I,:)];
   XN(I,:) = AA(1,:);
   YN(I,:) = AA(2,:);
   ZN(I,:) = AA(3,:);
end
