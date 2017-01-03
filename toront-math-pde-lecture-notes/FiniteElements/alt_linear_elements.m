function u = linear_elements(x);

N = length(x)-1;
dx = diff(x);

K = zeros(N,N);

K(1,1) = 1/dx(1) + 1/dx(2);
K(1,2) = -1/dx(2);
for j=2:N-1
  K(j,j-1) = -1/dx(j);
  K(j,j) = 1/dx(j) + 1/dx(j+1);
  K(j,j+1) = -1/dx(j+1);
end
K(N,N-1)= -1/dx(N);
K(N,N)= 1/dx(N);

% for f(x) = (x-1/2)^(1/3)
for j=1:N-1
    x1 = x(j+1);
    dx1 = dx(j);
    dxp1 = dx(j+1);
    F(j)=4*dxp1*nthroot(-4+8*x1-8*dx1,3)*x1^2-8*dxp1*nthroot(-4+8*x1-8*dx1,3)*x1*dx1-4*dxp1*nthroot(-4+8*x1-8*dx1,3)*x1+4*dxp1*nthroot(-4+8*x1-8*dx1,3)*dx1^2+4*dxp1*nthroot(-4+8*x1-8*dx1,3)*dx1+dxp1*nthroot(-4+8*x1-8*dx1,3)-4*dxp1*nthroot(8*x1-4,3)*x1^2+4*nthroot(8*x1-4,3)*x1*dxp1-dxp1*nthroot(8*x1-4,3)-4*dx1*nthroot(8*x1-4,3)*x1^2+4*nthroot(8*x1-4,3)*x1*dx1-dx1*nthroot(8*x1-4,3)+4*dx1*nthroot(8*x1+8*dxp1-4,3)*x1^2+8*dx1*nthroot(8*x1+8*dxp1-4,3)*x1*dxp1-4*dx1*nthroot(8*x1+8*dxp1-4,3)*x1+4*dx1*nthroot(8*x1+8*dxp1-4,3)*dxp1^2-4*dx1*nthroot(8*x1+8*dxp1-4,3)*dxp1+dx1*nthroot(8*x1+8*dxp1-4,3);
    F(j)=F(j)*9/(224*dx1*dxp1);

end
x1 = x(N+1);
dxn = dx(N);
F(N)=12*nthroot(-4+8*x1-8*dxn,3)*x1^2-24*nthroot(-4+8*x1-8*dxn,3)*x1*dxn-12*nthroot(-4+8*x1-8*dxn,3)*x1+12*nthroot(-4+8*x1-8*dxn,3)*dxn^2+12*nthroot(-4+8*x1-8*dxn,2)*dxn+3*nthroot(-4+8*x1-8*dxn,3)-12*nthroot(8*x1-4,3)*x1^2+28*nthroot(8*x1-4,3)*x1*dxn+12*nthroot(8*x1-4,3)*x1-14*nthroot(8*x1-4,3)*dxn-3*nthroot(8*x1-4,3);
F(N) = F(N)*3/(112*dxn);

% want to sove U*K = F
% that is,  U = F*inv(K) = F/K
U = F/K;
u(2:N+1) = U;
