% termbound_ex_script.m
% An initial/boundary problem example in 2D
% Assumes k=0.

h=10.^(-3); % Timestep

T=1; % Terminal Time

ntimes=4;
times=linspace(0,T,ntimes); % Obtain estimates at these times

nx=100; ny=100; ne=10^3;

xs=linspace(-5,5,nx);
ys=linspace(-5,5,ny);

b=[1;1];
sig=[1,.2;.2,2];
bh=h.*b;
sigh=sqrt(h).*sig;

for tt=1:ntimes

    ut=zeros(nx,ny);

    for i=1:nx % For each x grid-point
        for j=1:ny % For each y grid-point
            xt=[xs(i);ys(j)];
            if ((xt(1)^2+xt(2)^2)>=5^2)||(xt(1)^4+xt(2)^4<=2.5^4)
                ut(i,j)=NaN;
            else
                for k=1:ne % For each realization of the process
        
                    notstopped=true;
                    t=times(tt); X=xt; intg=0;
                    if t<T
                        while (notstopped)
                            t=t+h;
        
                            X=X+bh+sigh*randn(2,1); % Euler Approx.
            
                            if ((X(1)^2+X(2)^2)>=5^2)||(X(1)^4+X(2)^4<=2.5^4)||(t>=T)
                                notstopped=false;
                            end
        
                            intg=intg+h*exp(T-t)*sin(X(1))*cos(X(2)); % Riemann approx.
                        end
                    end
                    
                    if t<T
                        ut(i,j)=ut(i,j)+sin((T-t)*X(1)*X(2))+intg;
                    else
                        ut(i,j)=ut(i,j)+intg;
                    end
                end
                ut(i,j)=ut(i,j)/ne;
            end
        end
    end

    figure,surf(xs,ys,ut');
    view([0,90])
    colormap('gray');
    
end
