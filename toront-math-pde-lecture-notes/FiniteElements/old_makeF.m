function F = makeF(x,dx);

N = length(x)-1;
% for f(x) = cos(3*pi*x) 
for j=1:N-1
    x1 = x(j+1);
    dx1 = dx(j);
    dxp1 = dx(j+1);

    F(j) = 1/9*(-16*cos(pi*x1)^3*cos(pi*dx1)^3*dxp1+12*cos(pi*x1)^3*cos(pi*dx1)*dxp1+12*cos(pi*x1)*cos(pi*dx1)^3*dxp1-9*cos(pi*x1)*cos(pi*dx1)*dxp1-16*sin(pi*x1)*cos(pi*x1)^2*sin(pi*dx1)*cos(pi*dx1)^2*dxp1+4*sin(pi*x1)*cos(pi*x1)^2*sin(pi*dx1)*dxp1+4*sin(pi*x1)*sin(pi*dx1)*cos(pi*dx1)^2*dxp1-sin(pi*x1)*sin(pi*dx1)*dxp1+4*cos(pi*x1)^3*dxp1-3*cos(pi*x1)*dxp1+4*cos(pi*x1)^3*dx1-3*cos(pi*x1)*dx1-16*cos(pi*x1)^3*cos(pi*dxp1)^3*dx1+12*cos(pi*x1)^3*cos(pi*dxp1)*dx1+12*cos(pi*x1)*cos(pi*dxp1)^3*dx1-9*cos(pi*x1)*cos(pi*dxp1)*dx1+16*sin(pi*x1)*cos(pi*x1)^2*sin(pi*dxp1)*cos(pi*dxp1)^2*dx1-4*sin(pi*x1)*cos(pi*x1)^2*sin(pi*dxp1)*dx1-4*sin(pi*x1)*sin(pi*dxp1)*cos(pi*dxp1)^2*dx1+sin(pi*x1)*sin(pi*dxp1)*dx1)/(dx1*pi^2*dxp1);
end
x1 = x(N+1);
dxn = dx(N);
F(N) = 1/9*(-16*cos(pi*x1)^3*cos(pi*dxn)^3+12*cos(pi*x1)^3*cos(pi*dxn)+12*cos(pi*x1)*cos(pi*dxn)^3-9*cos(pi*x1)*cos(pi*dxn)-16*sin(pi*x1)*cos(pi*x1)^2*sin(pi*dxn)*cos(pi*dxn)^2+4*sin(pi*x1)*cos(pi*x1)^2*sin(pi*dxn)+4*sin(pi*x1)*sin(pi*dxn)*cos(pi*dxn)^2-sin(pi*x1)*sin(pi*dxn)+4*cos(pi*x1)^3-3*cos(pi*x1)+12*pi*dxn*sin(pi*x1)*cos(pi*x1)^2-3*pi*dxn*sin(pi*x1))/(dxn*pi^2);
