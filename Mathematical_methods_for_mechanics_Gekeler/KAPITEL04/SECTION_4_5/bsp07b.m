function F = bsp07b(Parmeter)
% Exakter Wert der Zielfunktion
alfa  = Parmeter(2);
sigma = 3 - sqrt(56 + 8*alfa)/4;
syms t
if alfa > - 7 & alfa <= -2.5
   f1 = 2 - t*t;
   f2 = 2 + t*t + 2*sigma*sigma - 4*sigma*t;
   F1 = int(f1,0,sigma);
   F2 = int(f2,sigma,3);
   F  = 2*(F1 + F2);
   F  = vpa(F,5);
end
if alfa > - 2.5 & alfa <= 0
   rho = 2*sigma;
   f1  = 2 - t*t;
   f2  = 2 + t*t + 2*sigma*sigma - 4*sigma*t;
   F1  = int(f1,0,sigma);
   F2  = int(f2,sigma,rho);
   F   = 2*(F1 + F2 + alfa*(3 - rho));
   F   = vpa(F,5);
end
