clear
m=101;
dx=0.010;
dx4=dx*dx*dx*dx;
ss=5.0;
Twall=0.0;
T=zeros(m);x=zeros(m);Txx=zeros(m);
x(1)=0.0;
T(1)=Twall; To(1)=Twall;
for i=1:m-1
  x(i+1)=x(i)+dx;
end
%nstep=10000000;
nstep=1000;
for kk=1:nstep
  T(1)=0.0; T(2)=0.0;
  T(m)=0.0; T(m-1)=0.0;
  for i=3:m-2
    T(i)=(dx4*ss-(T(i+2)-4.*T(i+1)-4.0*T(i-1)+T(i-2)))/6.0;
  end

end
Txx(1)=2.*(T(2)-T(1))/(dx*dx);
Txx(m)=2.*(T(m-1)-T(m))/(dx*dx);
for i=2:m-1
  Txx(i)=(T(i+1)-2.*T(i)+T(i-1))/(dx*dx);
end
figure(1)
plot(x,T)

title('Temperature')
xlabel('X')
ylabel('T')
