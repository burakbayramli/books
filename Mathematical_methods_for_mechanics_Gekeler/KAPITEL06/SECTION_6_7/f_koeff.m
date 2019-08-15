function [a,b,alfa,beta] = f_koeff(d3,D3,THETA1,THETA2,Parmeter)
% berechnet die Koeffizienten von
% f(x) = (alfa - beta*x)*(1 - x*x) - (a - b*x)^2;

T1     = Parmeter(1); T3   = Parmeter(2);
m      = Parmeter(3); gl   = Parmeter(4);
T1     = m*T1; T3 = m*T3;
a      = d3/T1; b = D3/T1;
if THETA1 == THETA2
  % alfa, beta aus f und Ableitung von f
  x = cos(THETA1);
  A = [1-x^2, -x*(1-x^2);
        -2*x, -1+3*x^2];
  B = [(d3 - D3*x )^2; -2*D3*(d3 - D3*x)];
  X = A\B; alfa = X(1); beta = X(2);
end
if THETA1 ~= 0 & THETA2 == pi/2
   alfa = a*a; x = cos(THETA1);
   beta = (alfa - (a-b*x)^2/(1 - x*x))/x;
end
if THETA1 == 0 & THETA2 == pi/2
   alfa = a*a; beta = 1; %beta FREI
end
if THETA1 == pi/2 & THETA2 ~= pi & THETA2 ~= pi/2;
   alfa = a*a; x = cos(THETA2);
   beta = (alfa - (a-b*x)^2/(1 - x*x))/x;
end
if THETA1 == pi/2 & THETA2 == pi
   alfa = a*a; beta = 2; % FREI
end
if THETA1 ~= THETA2
   if THETA1 ~= 0 & THETA2 & pi
      x1 = cos(THETA1); x2 = cos(THETA2);
      A = [1, - x1; 1, - x2];
      B = [(a - b*x1)^2/(1 - x1^2); (a - b*x2)^2/(1 - x2^2)];
      X = A\B; alfa = X(1); beta = X(2);
   end
end

