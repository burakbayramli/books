%vdpol.m
alpha = 1; sigma = 0.5;
a1 = @(x1,x2,t) x2;
a2 = @(x1,x2,t) x1*(alpha-x1^2)-x2;
b1 = @(x1,x2,t) 0 ;
b2 = @(x1,x2,t) sigma*x1;
n=10^6; h=10^(-3); t=h.*(0:1:n); x1=zeros(1,n+1); x2=x1;
x1(1)=-2;
x2(1)=0;
for k=1:n
   x1(k+1)=x1(k)+a1(x1(k),x2(k),t(k))*h+ ...
           b1(x1(k),x2(k),t(k))*sqrt(h)*randn;
   x2(k+1)=x2(k)+a2(x1(k),x2(k),t(k))*h+ ...
           b2(x1(k),x2(k),t(k))*sqrt(h)*randn;
end
step = 100; 
figure(1),plot(t(1:step:n),x1(1:step:n),'k-')
figure(2), plot(x1(1:step:n),x2(1:step:n),'k-');

