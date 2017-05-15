% Integration as expected value: Importance sampling
% integral of f(x), an appropriate p(x) PDF is taken

% the integrand functions
x=0:(pi/100):(0.25*pi); %domain of f(x)
xx=0:(pi/100):1; %domain of p(x)
f=25*(x.^3).*cos(2*x); %the function f(x)
alpha=4; beta=3; %parameters of the PDF
p=betapdf(xx,alpha,beta); %the function p(x) (beta PDF)

%Deterministic approximation of the integral
disp('deterministic integral result:');
DS=sum(f)*(pi/100) %print result

%display of f(x) and p(x)
figure(1)
plot(x,f,'k'); hold on;
plot(xx,p,'r');
axis([0 1 0 2.5]);
title('importance sampling: f(x) and p(x)');
xlabel('x');

%Monte Carlo Integration------------------------------------
%draw N samples from p(x) PDF
N=2000; %number of samples
x=random('beta',alpha,beta,1,N); %the samples

%evaluate g(x) at the samples
nv=0; %counter of valid data points
g=0; %initial value
for nn=1:N,
   if x(nn)>0, %avoid division by zero
      if x(nn)<=(0.25*pi), %values inside f() domain
         f=25*(x(nn).^3).*cos(2*x(nn)); %evaluate the function f() at xi
      else
         f=0;
      end;
      p=betapdf(x(nn),alpha,beta);
      g=g+(f/p); %adding
      nv=nv+1;
   end;
end;   

%integral
disp('Monte Carlo integral result:');
S=(g/nv) %print result

