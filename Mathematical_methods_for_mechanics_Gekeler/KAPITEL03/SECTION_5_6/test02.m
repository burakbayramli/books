function test02
% Computation for Ex. 3
format long g, format compact
syms y
f = 30*(1-4*y)^2*(1-y)^2 - 81*(1-y)^2 - 16*(1 - 4*y)^2;
f = simplify(f)

-67-10*y+653*y^2-1200*y^3+480*y^4
P = [480, -1200, 653, -10,-67];

WURZELN = roots(P)
X = zeros(1,4); Y = X;
for I = 1:4
    X(I) = (8 + 4*WURZELN(I))/(1 - 4*WURZELN(I));
    Y(I) = (1 + 3*WURZELN(I))/(1 - WURZELN(I));
end
X    
Y


XX = [X;Y]; Z = zeros(3,4);
for I = 1:4
   Z(:,I) = bsp03(XX(:,I),2,[]);
end
Z
X_OPT = [3.45455547048392;  0.186994753742436];     
    