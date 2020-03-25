% dirichlet_ex_script.m
% A Dirichlet example in 2D.
% Assumes k=0.

h=10.^(-3); % Timestep
nx=100; ny=100; ne=10^3;
xs=linspace(-5,5,nx);
ys=linspace(-5,5,ny);

u=zeros(nx,ny);

b=[1;1]; sig=[1,.2;.2,2];
bh=b.*h; sigh=sqrt(h).*sig;

for i=1:nx % For each x grid-point
    for j=1:ny % For each y grid-point
        x0=[xs(i);ys(j)];
        if ((x0(1)^2+x0(2)^2)>=5^2)||(x0(1)^4+x0(2)^4<=2.5^4)
            u(i,j)=NaN;
        else
            for k=1:ne % For each realization of the process
    
                notstopped=true;
                t=0; X=x0; intg=0;
                while (notstopped)
                    t=t+h;

                    X=X+bh+sigh*randn(2,1); % Euler Approx.
    
                    % Check the Boundary condition
                    if ((X(1)^2+X(2)^2)>=5^2)||(X(1)^4+X(2)^4<=2.5^4)
                        notstopped=false;
                    end
    
                    intg=intg+h*sin(X(1))*cos(X(2)); % Riemann approx.
                end
                u(i,j)=u(i,j)+intg;
            end
            u(i,j)=u(i,j)/ne;
        end
    end
end

figure,surf(xs,ys,u')
