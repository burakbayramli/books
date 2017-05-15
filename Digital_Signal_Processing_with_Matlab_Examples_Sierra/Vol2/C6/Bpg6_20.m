% Example of Levi's approximation
% (frequency domain modelling)

% a simple test case -----------------
num=[1 0.2 1];
den1=[1 0.1 0.1];
den2=[1 0.5 7];
den=conv(den1,den2);
Ho=tf(num,den);

w=logspace(-1,1,100);

%frequency-domain response
Hm=freqresp(Ho,w); 
Hm=squeeze(Hm)';  % "measured" data to be fitted

%error function

erf=@(x)sum(abs((Hm.*((j*w).^4+x(1)*(j*w).^3+x(2)*(j*w).^2+x(3)*(j*w)+x(4)))...
    -(x(5)*(j*w).^2+x(6)*(j*w)+x(7))).^2);
[x,fiterr]=fminsearch(erf,ones(7,1));

esnum=[x(5) -x(6) x(7)];
esden=[1 -x(1) x(2) -x(3) x(4)];
Hes=tf(esnum,esden);

%figure(1)
bode(Ho,'rx'); hold on;
bode(Hes,'b');
grid
title('Bode diagram: (x) data, (continuous) approximation');
