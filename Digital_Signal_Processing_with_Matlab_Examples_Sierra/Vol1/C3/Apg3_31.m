%Example of MA behaviour

%coeffs. of polynomial C
c0=1; c1=0.8; c2=0.5; c3=0.3;
%variable initial values
y=0; e1=0; e2=0; e3=0;

Ni=200; %number of iterations
ry=zeros(1,Ni); %for storage of y values
ee=randn(1,Ni); %vector of random values
 
%iterations
for nn=1:Ni,
    e=ee(nn);
    y=(c0*e+c1*e1+c2*e2+c3*e3); %according with MA model
    ry(nn)=y;
    e3=e2; e2=e1; e1=e; %memory update
end;

figure(1)
plot(ry,'k');
title('evolution of model output');
xlabel('n');

LP=[c3 c2 c1 c0]; %lag polynomial
R=roots(LP);  %roots of the lag polynomial

figure(2)
plot(0,0,'y.'); hold on
line([-2 1.5],[0 0]); line([0 0],[-2 2]); %axes
m=0:(pi/100):2*pi; plot(cos(m),sin(m),'k'); %circle
plot(real(R),imag(R),'kx','MarkerSize',12); %roots
axis([-2 1.5,-2 2]);
title('Roots of lag polynomial, and the unit circle')

figure(3)
subplot(2,1,1)
[cv,lags]=xcov(ry,'biased');
plot(lags,cv,'k');
title('covariances')
subplot(2,1,2)
stem(lags(Ni-6:Ni+6),cv(Ni-6:Ni+6),'k'); hold on;
plot([-6 6],[0 0],'k');
title('zoom around index 0');

