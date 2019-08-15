function y = bsp03(p,e,Fall)
M = size(p,2); N = size(e,2);
switch Fall
case 1, % exakte Loesung ------------------
   y = zeros(M,1);
   for I = 1:M
      rr = p(1,I)^2 + p(2,I)^2;
      y(I) = (rr^3 - 2*rr^2 - 4*rr)*cos(2*atan2(p(2,I),p(1,I)))/10;
   end
case 2, % Lasten --------------------
   y = zeros(M,1);
   for I = 1:M
      rr = p(1,I)^2 + p(2,I)^2;
      y(I) = 384*rr*cos(2*atan2(p(2,I),p(1,I)))/10;
   end
case 3, % e1-Werte fuer 2.te Ableitung von p im Buch ---
   y = zeros(1,N);
   for I = 1:N
      y(I) = 2*cos(2*atan2(p(2,e(1,I)),p(1,e(1,I))));
   end
case 4  % e2-Werte fuer 2.te Ableitung von p im Buch ---
   y = zeros(1,N);
   for I = 1:N
      y(I) = 2*cos(2*atan2(p(2,e(2,I)),p(1,e(2,I))));
   end
end
