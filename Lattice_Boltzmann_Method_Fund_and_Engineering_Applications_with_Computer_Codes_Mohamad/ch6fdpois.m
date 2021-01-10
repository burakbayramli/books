clear
m=81;n=81;
xl=1.0;yl=1.0;
dx=xl/(m-1); dy=yl/(n-1);
dx2=dx*dx; dy2=dy*dy;
pi2=2.0*pi;
phio=zeros(m,n);x=zeros(m);y=zeros(n);phi=zeros(m,n);
phim=zeros(m);Z=zeros(n,m);
x(1)=0.0; y(1)=0.0;
for i=1:m-1

  x(i+1)=x(i)+dx;

end
for j=1:n-1

  y(j+1)=y(j)+dy;

end
cof=2.*(1./dx2+1./dy2);
ss=0.05;
nstep=10000;
for kk=1:nstep

  phio=phi;

  for j=2:n-1

    for i=2:m-1

      ss=phi(i,j)*phi(i,j)+sin(pi2*x(i))*cos(pi*y(j))+0.1;

      phix=(phi(i+1,j)+phi(i-1,j))/dx2;
      phiy=(phi(i,j+1)+phi(i,j-1))/dy2;

      phi(i,j)=(phix+phiy+ss)/cof;
    end

  end
end

				%Boundary condition:

phi(1,:)=0.0;
phi(m,:)=0.0;

phi(:,1)=0.0;
phi(:,n)=0.0;

error=norm(phi-phio);
error

				%rotating matrix for contour plotting

for j=1:n

  for i=1:m

    Z(j,i)=phi(i,j);

  end

end

for i=1:m

  phim(i)=phi(i,(n-1)/2);

end

figure(1)
plot(x,phim)

xlabel('X')
ylabel('T')

figure(2)
contour(Z,20)
