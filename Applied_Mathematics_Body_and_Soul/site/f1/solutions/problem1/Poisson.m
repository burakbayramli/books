function integral = Poisson(u, v, w, du, dv, dw, dx, ds, x, d, t, eq)

if eq == 1
  integral = du'*dv*dx + g(x,d,t)*u*v*ds;
else
  integral = f(x,d,t)*v*dx + (g(x,d,t)*gd(x,d,t) - gn(x,d,t))*v*ds;
end

%--- Conductivity (penalty factor) ---
function y = g(x, d, t)
y = 1e7;

%--- Dirichlet boundary condition ----
function y = gd(x, d, t)
y = 0;

%--- Neumann boundary condition ---
function y = gn(x, d, t)
y = 0;

%--- Right-hand side, source term ---
function y = f(x, d, t)
y = 5*pi^2*sin(pi*x(1))*sin(2*pi*x(2));
