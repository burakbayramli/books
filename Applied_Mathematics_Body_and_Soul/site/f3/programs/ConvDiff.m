function integral = ConvDiff(u, v, w, du, dv, dw, dx, ds, x, d, t, eq)

if eq == 1
  integral = ?;
else
  integral = ?;
end

%--- Convection ---
function b = g(x, d, t)
y = ?;

%--- Conductivity (penalty factor) ---
function y = g(x, d, t)
y = ?;

%--- Dirichlet boundary condition ----
function y = gd(x, d, t)
y = ?;

%--- Neumann boundary condition ---
function y = gn(x, d, t)
y = ?;

%--- Right-hand side, source term ---
function y = f(x, d, t)
y = ?;
