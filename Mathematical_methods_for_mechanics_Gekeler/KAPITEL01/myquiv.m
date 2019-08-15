function myquiv(X,Y,U,V,c,d,farbe,S)
n = length(X);
for i = 1:n
   A = [X(i);X(i)+U(i)];
   B = [Y(i);Y(i)+V(i)];
   plot(A,B,'Color',farbe,'Linewidth',S);
   hold on
   VEC1 = [U(i), V(i)];
   L = norm(VEC1);
   VEC1 = VEC1/L;
   VEC2 = [-VEC1(2),VEC1(1)];
   SPITZE = [A(2), B(2)];
   ENDE1 = SPITZE - c*L*VEC1 + d*L*VEC2;
   ENDE2 = SPITZE - c*L*VEC1 - d*L*VEC2;
   Z = [SPITZE; ENDE1;ENDE2;SPITZE];
   fill(Z(:,1),Z(:,2),farbe);
   hold on
end
