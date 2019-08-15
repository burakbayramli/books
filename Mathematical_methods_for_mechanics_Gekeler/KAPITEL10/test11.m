function test11
% Gradient fuer bisphaerische Koordinaten
clc
syms u v w
f = [sin(v)*cos(w), sin(v)*sin(w), sinh(u)]/(cosh(u) - cos(v));
%g = diff(f,'u')
%h1 = [-sinh(u)*cos(w)*sin(v), -sinh(u)*sin(w)*sin(v), 1 - cosh(u)*cos(v)]/(cosh(u) - cos(v))^2;
%diff = g - h1;
%diff = simplify(diff)
%g = diff(f,'v')
%h2 = [cos(w)*(cos(v)*cosh(u) - 1),sin(w)*(cos(v)*cosh(u) - 1), -sin(v)*sinh(u)]/(cosh(u)- cos(v))^2;
%diff = g-h2;
%diff = simplify(diff)
%g = diff(f,'w')
%h3 = [-sin(v)*sin(w)*(cosh(u)-cos(v)), sin(v)*cos(w)*(cosh(u)-cos(v)), 0]/(cosh(u)-cos(v))^2;
%diff = g -h3;
%diff = simplify(diff)
h1 = [-sinh(u)*cos(w)*sin(v); -sinh(u)*sin(w)*sin(v); 1 - cosh(u)*cos(v)]
h2 = [cos(w)*(cos(v)*cosh(u) - 1);sin(w)*(cos(v)*cosh(u) - 1); -sin(v)*sinh(u)]
h3 = [-sin(v)*sin(w)*(cosh(u)-cos(v)); sin(v)*cos(w)*(cosh(u)-cos(v)); 0]


