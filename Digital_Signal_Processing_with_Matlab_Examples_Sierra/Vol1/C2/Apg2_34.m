% Integration as expected value
% integral of g(x)*p(x), where p(x) can be taken as a PDF

% the integrand functions
x=0:(pi/100):(1.5*pi); %domain of the integral
g=(0.8*sin(x)).^2; % the function g(x)
mu=1; %parameter of the exponential distribution
p=exppdf(x,mu); %the function p(x) (exponential PDF)

%Deterministic approximation of the integral
aux=abs(g.*p);
disp('deterministic integral result:');
DS=sum(aux)*(pi/100) %print result

%display of the integrand functions
figure(1)
plot(x,g,'k'); hold on;
plot(x,p,'r');
plot(x,aux,'b');
for vx=10:10:151, %mark the integral area
   l=(vx*pi)/100;
   plot([l l],[0 aux(vx)],'g','linewidth',2);
end;   
axis([0 1.5*pi 0 1.2]);
title('Integral of the product g(x)p(x)');
xlabel('x');

%Monte Carlo Integration------------------------------------
%draw N samples from p(x) as PDF
N=3000; %number of samples
x=random('exp',mu,1,N); %the samples

%evaluate g(x) at the samples
nv=0; %counter of valid data points
L=1.5*pi; %limit of the integral
for nn=1:N,
   if x(nn)<=L, g(nn)=(0.8*sin(x(nn)))^2; nv=nv+1;
   else 
      g(nn)=0; %the value of x is outside integral domain
   end;
end;   

%integral
disp('Monte Carlo integral result:');
S=(sum(g)/nv) %print result

