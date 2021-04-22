function [kh,x,xj,xgs] = vib_string(k,o)

n=99;
L=1.0;
h=L/(n+1);
%k=2*pi;
kh=k*h

%Tri-Diagonal Linear System: Ax=b
x=[h:h:L-h]';
a=zeros(n,n);
f=zeros(n,1);
% Offdiagonal values
%o=1.0

a(1,1) =kh^2 - 2;
a(1,2)=o;

for i=2:n-1
    a(i,i)=a(1,1);
    a(i,i-1) = o;
    a(i,i+1) = o;
end
a(n,n)=a(1,1);
a(n,n-1)=o;

% Hanning window load
nf=round((n+1)/3);
nw=round((n+1)/6);
nw=min(min(nw,nf-1),n-nf);
figure(1)
hold off

nw1=nf-nw;
nw2=nf+nw;
f(nw1:nw2) = h^2*hanning(nw2-nw1+1);
subplot(2,1,1); p=plot(x,f,'r');set(p,'LineWidth',2);
p=title('Force Distribution');
set(p,'FontSize',14)

% Exact solution
y=inv(a)*f;
subplot(2,1,2); p=plot(x,y,'b');set(p,'LineWidth',2);
p=legend(['Off-diag. = ' num2str(o)]);
set(p,'FontSize',14);
p=title('String Displacement (Exact)');
set(p,'FontSize',14);
p=xlabel('x');
set(p,'FontSize',14);


%Iterative solution using Jacobi's and Gauss-Seidel's methods
b=-a;
c=zeros(n,1);
for i=1:n
    b(i,i)=0;
    for j=1:n
        b(i,j)=b(i,j)/a(i,i);
        c(i)=f(i)/a(i,i);
    end    
end

nj=100;
xj=f;
xgs=f;

figure(2)
nc=6
col=['r' 'g' 'b' 'c' 'm' 'y']
hold off
for j=1:nj
    % jacobi
    xj=b*xj+c;
    % gauss-seidel
    xgs(1)=b(1,2:n)*xgs(2:n) + c(1);
    for i=2:n-1
        xgs(i)=b(i,1:i-1)*xgs(1:i-1) + b(i,i+1:n)*xgs(i+1:n) +c(i);
    end
    xgs(n)= b(n,1:n-1)*xgs(1:n-1) +c(n);
    cc=col(mod(j-1,nc)+1);
    subplot(2,1,1); plot(x,xj,cc); hold on;
    p=title('Jacobi');
    set(p,'FontSize',14);    
    subplot(2,1,2); plot(x,xgs,cc); hold on;
    p=title('Gauss-Seidel');
    set(p,'FontSize',14);
    p=xlabel('x');
    set(p,'FontSize',14);
end
subplot(2,1,1); plot(x,xj,'+'); hold on;
subplot(2,1,2); plot(x,xgs,'+'); hold on;
    