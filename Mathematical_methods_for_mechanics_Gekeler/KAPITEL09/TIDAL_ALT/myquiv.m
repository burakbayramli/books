function myquiv(X,Y,U,V,farbe)

a = 15000; TOL = 1.0E-4; n = length(X);
for i = 1:n
   A = [X(i);X(i)+a*U(i)]; B = [Y(i);Y(i)+a*V(i)];
   LL= sqrt(U(i)*U(i) + V(i)*V(i));
   if LL < TOL
      plot(X(i),Y(i),'r.'), hold on
   end
   if LL >= TOL
      U1 = [U(i);V(i)]/LL; V1 = [-U1(2); U1(1)];
      plot(A,B,farbe), hold on
      B1 = [A(2);B(2)];
      c = 0.3*a*LL; d = 0.05*a*LL;
      C = B1 - c*U1 + d*V1; D = B1 - c*U1 - d*V1;
      Z = [B1, C, D, B1];
      fill(Z(1,:),Z(2,:),farbe), hold on
   end
end
