function ch2ex5
global ixindices mc
% Spatial grid, initial values:
N = 128; 
x = (2*pi/N)*(1:N); 
v0 = exp(-100*(x - 1) .^ 2);
% Quantities passed as global variables to f(t,v):
ixindices = i * [0:N/2-1 0 -N/2+1:-1]';
mc = - (0.2 + sin(x - 1) .^ 2)';
% Integrate and plot:
t = 0:0.30:8;
[t,v] = ode23(@f,t,v0);
colormap(gray)
surf(x,t,v), view(10,70), axis([0 2*pi 0 8 0 5])
%surf(x,t,v), view(10,70), axis([0 2*pi 0 8 0 5])
ylabel t, zlabel u, grid off
%========================================================
function dvdt = f(t,v)
global ixindices mc
dvdt = mc .* real(ifft(ixindices .* fft(v)));