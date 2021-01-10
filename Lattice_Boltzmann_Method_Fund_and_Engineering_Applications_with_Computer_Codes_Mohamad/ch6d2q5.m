clear
m=101;n=101;
ms=real(m-1)*(m-1);
xl=1.0;yl=1.0;
dx=xl/(m-1.0); dy=yl/(n-1.0);
w0=2./6.;
w=1./6.;
c2=1./3.;
dx=1.0;
pi2=2.*pi;
f0=zeros(m,n);f1=zeros(m,n);f2=zeros(m,n);f3=zeros(m,n);f4=zeros(m,n);
rho=zeros(m,n);x=zeros(m);y=zeros(n);
Z=zeros(n,m);
x(1)=0.0; y(1)=0.0;
for i=1:m-1
  x(i+1)=x(i)+dx;
end

for j=1:n-1
  y(j+1)=y(j)+dy;
end
ss=0.0/ms;
nstep=80000;
for i=1:m
  f0(i,j)=w0*rho(i,j);
  f1(i,j)=w*rho(i,j);
  f2(i,j)=w*rho(i,j);
  f3(i,j)=w*rho(i,j);
  f4(i,j)=w*rho(i,j);

end
				%Collision:
for k1=1:nstep
  for j=1:n
    for i=1:m

		   % For variable source term, activate the line below
		   %

      ss=(rho(i,j)*rho(i,j)+sin(pi2*i/m)*cos(pi*j/n)+0.1)/ms;

      feq0=w0*rho(i,j);
      feq=w*rho(i,j);
      f0(i,j)=feq0+w0*ss/6.0;
      f1(i,j)=feq+w*ss/6.0;
      f2(i,j)=feq+w*ss/6.;
      f3(i,j)=feq+w*ss/6.0;
      f4(i,j)=feq+w*ss/6.0;

    end
  end

				% Streaming:
  f1(:,:)=circshift(squeeze(f1(:,:)),[+1,0]);
  f2(:,:)=circshift(squeeze(f2(:,:)),[-1,0]);
  f3(:,:)=circshift(squeeze(f3(:,:)),[0,-1]);
  f4(:,:)=circshift(squeeze(f4(:,:)),[0,+1]);

				%Boundary condition:

  for j=1:n

    f1(1,j)=-f2(1,j)-f0(1,j)-f3(1,j)-f4(1,j);

    f1(m,j)=-f2(m,j)-f0(m,j)-f3(m,j)-f4(m,j);
  end
  for i=1:m

    f4(i,1)=-f0(i,1)-f1(i,1)-f2(i,1)-f3(i,1);
    f3(i,n)=sin(pi*i/m)-f0(i,n)-f4(i,n)-f2(i,n)-f1(i,n);

  end

  for j=1:n   
    for i=1:m

      rho(i,j)=f1(i,j)+f2(i,j)+f0(i,j)+f3(i,j)+f4(i,j);

    end

  end

end
				%rotating matrix for contour plotting
for j=1:n

  for i=1:m

    Z(j,i)=rho(i,j);

  end

end

figure(1)

plot(x,rho(:,(n-1)/2))

xlabel('X')
ylabel('T')

figure(2)
contour(Z,20)
clear
m=81;n=81;
ms=real(m-1)*(m-1);
xl=1.0;yl=1.0;


dx=xl/(m-1.0); dy=yl/(n-1.0);
w0=2./6.;
w=1./6.;
c2=1./3.;
dx=1.0;
pi2=2.*pi;
f0=zeros(m,n);f1=zeros(m,n);f2=zeros(m,n);f3=zeros(m,n);f4=zeros(m,n);
rho=zeros(m,n);x=zeros(m);y=zeros(n);gho=zeros(m,n);
Z=zeros(n,m);
x(1)=0.0; y(1)=0.0;
for i=1:m-1

  x(i+1)=x(i)+dx;

end
for j=1:n-1

  y(j+1)=y(j)+dy;

end
ss=1.0/ms;
				%nstep=10000;
nstep=100;
for i=1:m

  f0(i,j)=w0*rho(i,j);
  f1(i,j)=w*rho(i,j);
  f2(i,j)=w*rho(i,j);
  f3(i,j)=w*rho(i,j);
  f4(i,j)=w*rho(i,j);

end
				%Collision:
for k1=1:nstep

  for j=1:n
    for i=1:m

		   % For variable source term, activate the line below

      ss=-(sin(pi*i/m)*sin(pi*j/n))/ms;

      feq0=w0*rho(i,j);
      feq=w*rho(i,j);
      f0(i,j)=feq0+w0*ss/6.0;
      f1(i,j)=feq+w*ss/6.0;
      f2(i,j)=feq+w*ss/6.;
      f3(i,j)=feq+w*ss/6.0;
      f4(i,j)=feq+w*ss/6.0;

    end
  end

  f1(:,:)=circshift(squeeze(f1(:,:)),[+1,0]);
  f2(:,:)=circshift(squeeze(f2(:,:)),[-1,0]);
  f3(:,:)=circshift(squeeze(f3(:,:)),[0,-1]);
  f4(:,:)=circshift(squeeze(f4(:,:)),[0,+1]);

  for j=1:n

    f1(1,j)=-f2(1,j)-f0(1,j)-f3(1,j)-f4(1,j);

    f1(m,j)=-f2(m,j)-f0(m,j)-f3(m,j)-f4(m,j);
  end
  for i=1:m

    f4(i,1)=-f0(i,1)-f1(i,1)-f2(i,1)-f3(i,1);
    f3(i,n)=-f0(i,n)-f4(i,n)-f2(i,n)-f1(i,n);

  end

  for j=1:n   
    for i=1:m

      rho(i,j)=f1(i,j)+f2(i,j)+f0(i,j)+f3(i,j)+f4(i,j);

    end

  end

end
				%scaling rho

				%D2w=q
for k1=1:nstep

  for j=1:n
    for i=1:m

		   % For variable source term, activate the line below

      ss=-rho(i,j)/(ms);
      geq0=w0*gho(i,j);
      geq=w*gho(i,j);
      g0(i,j)=geq0+w0*ss/6.0;
      g1(i,j)=geq+w*ss/6.0;
      g2(i,j)=geq+w*ss/6.;
      g3(i,j)=geq+w*ss/6.0;
      g4(i,j)=geq+w*ss/6.0;

    end
  end

				% Streaming:
  g1(:,:)=circshift(squeeze(g1(:,:)),[+1,0]);
  g2(:,:)=circshift(squeeze(g2(:,:)),[-1,0]);
  g3(:,:)=circshift(squeeze(g3(:,:)),[0,-1]);
  g4(:,:)=circshift(squeeze(g4(:,:)),[0,+1]);

				%Boundary condition:
  for j=1:n

    g1(1,j)=-g2(1,j)-g0(1,j)-g3(1,j)-g4(1,j);

    g1(m,j)=-g2(m,j)-g0(m,j)-g3(m,j)-g4(m,j);
  end
  for i=1:m

    g4(i,1)=-g0(i,1)-g1(i,1)-g2(i,1)-g3(i,1);
    g3(i,n)=-g0(i,n)-g4(i,n)-g2(i,n)-g1(i,n);

  end

  for j=1:n
    
    for i=1:m

      gho(i,j)=g1(i,j)+g2(i,j)+g0(i,j)+g3(i,j)+g4(i,j);

    end

  end

end

for j=1:n


  for i=1:m

    Z(j,i)=gho(i,j);

  end

end
figure(1)
plot(x/(m-1),gho(:,(n-1)/2))

xlabel('X')
ylabel('T')

figure(2)
contour(Z,20)
