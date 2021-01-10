clear
u=0.10;
m=101;
w0=4./6.;
w1=1./6.;
c2=1./3.;
w2=w1;
dx=1.0;
rho=zeros(m);f0=zeros(m);f1=zeros(m);f2=zeros(m);
x=zeros(m);
x(1)=0.0;
for i=1:m-1

  x(i+1)=x(i)+dx;

end
alpha=0.25;
omega=1/(alpha+0.5);
twall=1.0;
nstep=400;
for i=1:m

  f0(i)=w0*rho(i);
  f1(i)=w1*rho(i);
  f2(i)=w2*rho(i);

end
				%Collision:
for k1=1:nstep

  for i=1:m

    feq0=w0*rho(i);
    feq1=w1*rho(i)*(1.+3.*u);
    feq2=w2*rho(i)*(1.-3.*u);
    f0(i)=(1-omega)*f0(i)+omega*feq0;
    f1(i)=(1-omega)*f1(i)+omega*feq1;
    f2(i)=(1-omega)*f2(i)+omega*feq2;

  end

				% Streaming:
  for i=1:m-1

    f1(m-i+1)=f1(m-i);
    f2(i)=f2(i+1);

  end
				%Boundary condition:


  f1(1)=twall-f2(1)-f0(1);
  f1(m)=f1(m-1);
  f2(m)=f2(m-1);
  f0(m)=f0(m-1);
  for j=1:m

    rho(j)=f1(j)+f2(j)+f0(j);

  end

end
figure(1)
plot(x,rho)

title('Temperature, nstep=400')
xlabel('X')
ylabel('T')

