function integral = ConvDiff(u, v, w, du, dv, dw, dx, ds, x, d, t, eq)

if eq == 1
  integral = du'*dv*dx + [20 0]*du*v*dx + g(x,d,t)*u*v*ds;
else
  integral = f(x,d,t)*v*dx + (g(x,d,t)*gd(x,d,t) - gn(x,d,t))*v*ds;
end

%--- Conductivity (penalty factor) ---
function y = g(x, d, t)

if d == 1
  y = 0;
else
  y = 1e7;
end

%--- Dirichlet boundary condition ----
function y = gd(x, d, t)

if d == 5
  y = 10;
else
  y = 0;
end

%--- Neumann boundary condition ---
function y = gn(x, d, t)
y = 0;

%--- Right-hand side, source term ---
function y = f(x, d, t)
y = 0;
