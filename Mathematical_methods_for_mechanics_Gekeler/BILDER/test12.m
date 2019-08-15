function test12
% Polarkoordinaten
syms x y
f = acos(x/sqrt(x^2 + y^2));
fx = diff(f,'y')
fx = simplify(fx);
fx = simplify(fx)

