
clear 
close all

% Program to show the interpolation using Lagrange polynomials

% Initialize space [-1 1]
nx=1000;
x=linspace(-1,1,nx)

% Initialize arbitrary function
% Example function: sum over 5 sin function
n = 5
a = -[ .5 1 -3 -2 -5 4] ;
f=x*0;
for i=1:n,
f = f + sin(pi/a(i)*x);
end 

% Interpolate this function with Lagrange Polynomials of various degree,
% calculate the misfic energy

% Get order of Lagrange polynomial
N=input(' Give polynomials degree (N) : ');

% Get collocation points xi from gll routine   
[xi,w] = gll(N);
    
% Initialize known function at collocation points
fi=xi*0;
for i=1:n
    fi = fi+ sin(pi/a(i)*xi);
end 

% Initialize Lagrange polynomials on fine grid
for i=1:length(x);
    for j=0:N,
            lp(j+1,i)=lagrange(N,j,x(i));
    end
end

% Calculate interpolating polynomials by mulitplying 
% Lagrange polynomials with function values at xi
s=x*0;
for i=1:N+1   
    s=s+lp(i,:)*fi(i);
end

%Plot results    
plot(x,s,'k--'), hold on
plot(x,f,'k-')
plot(xi,fi,'s')
legend('Interpolating function','Original Function','Collocation points','Location','Best')
xlabel('x')
ylabel('f(x)')

hold off
axis square
%set(gca,'YTick',[])
    

