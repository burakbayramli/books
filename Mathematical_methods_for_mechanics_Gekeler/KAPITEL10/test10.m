
function test10
% Gradient fuer Toruskoordinaten
clc
syms u v w
f = [sinh(u)*cos(w), sinh(u)*sin(w), sin(v)]/(cosh(u) - cos(v));
%g = diff(f,'u');
%h1 = [cos(w)*(1 - cosh(u)*cos(v)), sin(w)*(1 - cosh(u)*cos(v)), -sinh(u)*sin(v)]/(cosh(u) - cos(v))^2;
%diff = g - h ;
%diff = simplify(diff);
%g = diff(f,'v')
%h2 = [-sinh(u)*sin(v)*cos(w), -sinh(u)*sin(v)*sin(w), (cosh(u)*cos(v)-1)]/(cosh(u)- cos(v))^2;
%diff = g-h;
%diff = simplify(diff)
%g = diff(f,'w')
%h3 = [-sinh(u)*sin(w)*(cosh(u)-cos(v)), sinh(u)*cos(w)*(cosh(u)-cos(v)), 0]/(cosh(u)-cos(v))^2;
%diff = g -h;
%diff = simplify(diff)
h1 = [cos(w)*(1 - cosh(u)*cos(v)); sin(w)*(1 - cosh(u)*cos(v)); -sinh(u)*sin(v)]
h2 = [-sinh(u)*sin(v)*cos(w); -sinh(u)*sin(v)*sin(w); (cosh(u)*cos(v)-1)]
h3 = [-sinh(u)*sin(w)*(cosh(u)-cos(v)); sinh(u)*cos(w)*(cosh(u)-cos(v)); 0]


