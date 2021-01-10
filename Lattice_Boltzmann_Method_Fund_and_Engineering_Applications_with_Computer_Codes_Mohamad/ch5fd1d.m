clear
m=101;
dx=1.;
dx2=dx*dx;
dt=0.5;
alpha=0.50;
Twall=1.0;
T=zeros(m);To=zeros(m);
x=zeros(m);fluxq=zeros(m);
x(1)=0.0;
T(1)=Twall; To(1)=Twall;

for i=1:m-1
  x(i+1)=x(i)+dx;
end

nstep=400;

for kk=1:nstep
  for i=2:m-1
    T(i)=To(i)+dt*alpha*(To(i+1)-2.*To(i)+To(i-1))/dx2;
  end
				%update
  for k=2:m-1
    To(k)=T(k);
  end
  To(m)=T(m-1);
end

				%Flux:

for k=1:m-1
  fluxq(k)=(To(k)-To(k+1)/dx);
end

fluxq(m)=fluxq(m-1);

figure(1)
plot(x,To)
title('Temperature')
xlabel('X')
ylabel('T')
figure(2)
plot(x,fluxq,'x')
title('Flux')
xlabel('X')
ylabel('Flux')
