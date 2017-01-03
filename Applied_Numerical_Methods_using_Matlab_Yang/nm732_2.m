%nm732_2: uses fminimax() for a vector-valued objective ftn f(x)
clear, clf
f=inline('1./(1+8*x.*x)','x');
f73221=inline('abs(polyval(a,x)-fx)','a','x','fx');
f73222=inline('polyval(a,x)-fx','a','x','fx');
N=2; % the degree of approximating polynomial
a0=zeros(1,N+1); %initial guess of polynomial coefficients
xx=-2+[0:200]'/50; %intermediate points 
fx=feval(f,xx);    % and their function values f(xx)
ao_m=fminimax(f73221,a0,[],[],[],[],[],[],[],[],xx,fx) %fminimax sol
for n=1:N+1, C(:,n)=xx.^(N+1-n); end
ao_ll=lsqlin(C,fx) %linear LS to minimize (Ca-fx)^2 with no constraint
ao_ln=lsqnonlin(f73222,a0,[],[],[],xx,fx) %nonlinear LS 
c2=cheby(f,N,-2,2) %Chebyshev polynomial over [-2,2]
plot(xx,fx,':', xx,polyval(ao_m,xx),'m', xx,polyval(ao_ll,xx),'r')
hold on, plot(xx,polyval(ao_ln,xx),'b', xx,polyval(c2,xx),'--')
axis([-2 2 -0.4 1.1])
