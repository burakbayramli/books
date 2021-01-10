clear
m=101;
ms=real((m-1)*(m-1))
ms2=ms*ms;
xl=1.0;
dx=xl/(m-1.0);
w0=4./6.;
w=1./6.;
c2=1./3.;
dx=1.0;
f0=zeros(m);f1=zeros(m);f2=zeros(m);
rho=zeros(m);x=zeros(m);gho=zeros(m);an=zeros(m);
Z=zeros(m);
x(1)=0.0;
for i=1:m-1
  x(i+1)=x(i)+dx;
end

ss=-1.0/ms;
%nstep=100000;
nstep=1000;

for i=1:m
  f0(i)=w0*rho(i);
  f1(i)=w*rho(i);
  f2(i)=w*rho(i);

end

				%Collision:
for k1=1:nstep
  for i=1:m
    feq0=w0*rho(i);
    feq=w*rho(i);
    f0(i)=feq0+w0*c2*ss/2.0;
    f1(i)=feq+w*c2*ss/2.0;
    f2(i)=feq+w*c2*ss/2.0;

  end

				% Streaming:
  f1(:)=circshift(squeeze(f1(:)),[+1,0]);
  f2(:)=circshift(squeeze(f2(:)),[-1,0]);
				%Boundary condition:
  f1(1)=1./12.-f2(1)-f0(1);
  f2(m)=1./12.-f1(m)-f0(m);

  for i=1:m
    
    rho(i)=f1(i)+f2(i)+f0(i);

  end

end

				%D2w=q
for k1=1:nstep

  for i=1:m

		   % For variable source term, activate the line below

    ss2=-rho(i)/ms;
    geq0=w0*gho(i);
    geq=w*gho(i);
    g0(i)=geq0+w0*c2*ss2/2.0;
    g1(i)=geq+w*c2*ss2/2.0;
    g2(i)=geq+w*c2*ss2/2.0;

  end

				% Streaming:
  g1(:)=circshift(squeeze(g1(:)),[+1,0]);
  g2(:)=circshift(squeeze(g2(:)),[-1,0]);

				%Boundary condition:

  g1(1)=-g2(1)-g0(1);

  g2(m)=-g1(m)-g0(m);

  for i=1:m

    gho(i)=g1(i)+g2(i)+g0(i);

  end

end

figure(1)

plot(x/(m-1),gho)

xlabel('X')
ylabel('T')

hold
				%analytical solution
for i=1:m

  sx=x(i)/(m-1);
  x4=sx*sx*sx*sx;
  x3=sx*sx*sx;
  x2=sx*sx;

  an(i)=x4/24.-x3/12.+x2/24;
end

plot(x/(m-1),an,'ks')
