function Y1 = prs(X,Y,r,s)
% Computes Integrals P_rs of Table 2.3
SX  = sum(X)/3; SY  = sum(Y)/3;
XX   = X + SX;   YY   = Y + SY;
X1  = XX(1); X2 = XX(2); X3 = XX(3);
Y1  = YY(1); Y2 = YY(2); Y3 = YY(3);
F   = ((X2-X1)*(Y3-Y1) - (X3-X1)*(Y2-Y1))/2;
U = F*(X1^r*Y1^s + X2^r*Y2^s + X3^r*Y3^s);
if r+s == 0, Y1  = F;      end
if r+s == 1, Y1 = 0;       end
if r+s == 2, Y1 = U/12;    end
if r+s == 3, Y1 = U/30;    end
if r+s == 4, Y1 = U/30;    end
if r+s == 5, Y1 = 2*U/105; end
