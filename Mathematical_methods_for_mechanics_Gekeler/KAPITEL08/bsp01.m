function y = bsp01(p,e,Fall)
M = size(p,2); N = size(e,2);
switch Fall
case 1, % exakte Loesung ------------------
   rr = p(1,:).^2 + p(2,:).^2;
   y  = ((1-rr).*(5 - rr))';
case 2, % Lasten --------------------
   y       = 64*ones(M,1); 
case 3, % e1-Werte fuer 2.te Ableitung von p im Buch ---
   y = zeros(1,N);
case 4  % e2-Werte fuer 2.te Ableitung von p im Buch ---
   y = zeros(1,N);
end
