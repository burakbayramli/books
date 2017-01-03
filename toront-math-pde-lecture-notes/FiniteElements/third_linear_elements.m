function [x,u] = linear_elements(N);

h = 1/N;
x = 0:h:1;
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

% for f(x) = 1 if x >= 1/2
for j=1:N-1
    x1 = x(j+1);
    dx1 = dx(j);
    dxp1 = dx(j+1);
    if x1+dxp1 < 1/2
        % the support [x1-dx1,x1+dxp1] doesn't intersect [1/2,1]
        F(j) = 0;
    elseif x1 < 1/2
        % x1 < 1/2 < x1+dxp1
        F(j) = 1/8*(2*x1+2*dxp1-1)^2/dxp1;
    else
        if x1-dx1 < 1/2
            % x1-dx1 < 1/2 < x1
            F(j) =1/8*(-4*x1^2-1+4*x1+8*x1*dx1-4*dx1+4*dx1*dxp1)/dx1;
        else
            % 1/2 < x1-dx1
            F(j) = 1/2*dx1+1/2*dxp1;
        end
    end
end
x1 = x(N+1);
dxn = dx(N);
F(N) = 1/2*dxn;

% want to sove U*K = F
% that is,  U = F*inv(K) = F/K
U = F/K;
u(2:N+1) = U;

% the exact solution is (x/2).*(x<1/2) + (-1/2*x.^2+x-1/8).*(x>=1/2);
