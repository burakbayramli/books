clear
m=201;
tp=zeros(m);tpo=zeros(m);x=zeros(m);
u=0.10;
dx=0.50;
dt=0.25;
alpha=0.25;
mstep=1600;
x(1)=0.0;
for i=1:m-1
  x(i+1)=x(i)+dx;
end
twall=1.0;
tp(1)=twall; tpo(1)=twall;
for k1=1:mstep
  for i=2:m-1
    adv=dt*u*(tpo(i)-tpo(i-1))/dx;
    def=dt*alpha*(tpo(i+1)-2.*tpo(i)+tpo(i-1))/(dx*dx);
    tp(i)=tpo(i)+def-adv;
  end

  tp(1)=twall;
  tp(m)=tp(m-1);
  for i=1:m
    tpo(i)=tp(i);
  end
end

figure(1)
plot(x,tpo)

title('Temperature, nstep=400')
xlabel('X')
ylabel('T')
