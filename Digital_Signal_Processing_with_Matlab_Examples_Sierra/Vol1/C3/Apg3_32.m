%Example of AR behaviour

%coeffs. of polynomial A
a1=0.8; a2=0.5; a3=0.3;
%variable initial values
y=0; y1=0; y2=0; y3=0;

Ni=200; %number of iterations
ry=zeros(1,Ni); %for storage of y values
ee=randn(1,Ni); %vector of random values
 
%iterations
for nn=1:Ni,
    e=ee(nn);
    y=e-(a1*y1+a2*y2+a3*y3); %according with AR model
    ry(nn)=y;
    y3=y2; y2=y1; y1=y; %memory update
end;

figure(1)
plot(ry,'k');
title('evolution of model output');
xlabel('n');

LP=[a3 a2 a1 1]; %lag polynomial
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

