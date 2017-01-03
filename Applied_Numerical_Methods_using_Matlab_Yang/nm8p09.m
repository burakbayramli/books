%nm8p09.m to solve a set of differential eq.s, i.e., a state equation
clear, clf
global A
df= 'df861'; 
k1=5; k2=10; m1=1; m2=1; % the spring constants and the masses
A=[(k1+k2)/m1 -k2/m1; -k2/m2 k2/m2]; NA=size(A,2);
t0=0; tf=??; x0=[? ???? ? ?]; % initial/final time & initial values
[t4,x4]=ode45(df,[t0 tf],x0);
[V,LAMBDA]= eig(A); % modal matrix composed of eigenvectors
w0= x0(1:NA)*V; w10= x0(NA+1:end)*V; % Eq.(8.6-8)
omega=??????????????????; 
for n=1:NA % Eq.(8.6-7)
  w(:,n)=[cos(omega(n)*t4) sin(omega(n)*t4)]*[w0(n);w10(n)/omega(n)];
end
xE= w*V.'; % Eq.(8.6-3)
for n=1:NA
subplot(311+n), plot(t4,x4(:,n),'b', t4,xE(:,n),'r')
end
