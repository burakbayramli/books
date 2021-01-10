clear
m=101;n=101;
dx=0.010;
dy=0.010;
dx2=dx*dx;
dy2=dy*dy;
dxdy2=2./dx2+2./dy2;
ss=0.0;
omega=1.80;

error=5.0;tolr=0.00001;
count=0;
T=zeros(m,n);x=zeros(m);y=zeros(n);Txx=zeros(m,n);To=zeros(m,n);
x(1)=0.0;
y(1)=0.0;
for i=1:m-1
  x(i+1)=x(i)+dx;
end

for j=1:n-1
  y(j+1)=y(j)+dy;

end
				%boundary conditions
for j=1:n
  T(1,j)=0.0;
  T(m,j)=0.0;
end
for i=1:m
  T(i,1)=0.0;
  T(i,n)=sin(pi*(i-1)/(m-1));

end

nstep=10000;
while error>tolr

  for i=2:m-1
    for j=2:n-1
      txx=(T(i+1,j)-2.*T(i,j)+T(i-1,j))/dx2;
      tyy=(T(i,j+1)-2.*T(i,j)+T(i,j-1))/dy2;
      T(i,j)=T(i,j)+omega*(txx+tyy-ss)/dxdy2;
    end
  end

  count=count+1;
  error=0.;
  for i=1:m
    for j=1:n
      error=error+abs(T(i,j)-To(i,j));
    end
  end
  To=T;
  count;

end

figure(1)
plot(x,T(:,(n-1)/2))

title('Temperature')
xlabel('X')
ylabel('T')
for j=1:n
  for i=1:m
    Z(j,i)=T(i,j);
  end

end
figure(2)
contour(Z,20)
