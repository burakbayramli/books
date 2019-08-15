function ch2ex6
global c_h
% Spatial grid, initial values:
N = 1000;
h = 2*pi/N;
x = h*(1:N);
v0 = exp(-100*(x - 1) .^ 2);
c_h = - (0.2 + sin(x - 1) .^ 2)' / h;
% Sparsity pattern for the Jacobian:
S = sparse(N,N);
for m = 2:N
    S(m,m-1) = 1;
    S(m,m) = 1;
end
options = odeset('JPattern',S);
% Integrate and plot:
t = 0:0.30:8;
[t,v] = ode15s(@f,t,v0,options);
pltspace = ceil(N/128);
x = x(1:pltspace:end);
v = v(:,1:pltspace:end);
surf(x,t,v), view(10,70), axis([0 2*pi 0 8 0 5])
ylabel t, zlabel u, grid off
%================================================
function dvdt = f(t,v)
global c_h
dvdt = c_h .* [0; diff(v)];