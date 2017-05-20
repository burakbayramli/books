clf
R=[10 100 1000 6364.9]
L=10.1321;
C=0.000001;;
w=0:1:200*pi;
for i=1:4
T1=(-w.^2+1/(L*C))./(-w.^2+j*w*(R(i)/L)+1/(L*C));
Mag_T1=abs(T1);
plot(w,Mag_T1);
hold on
end
title('second order RLC series filter')
xlabel('w in rad');
ylabel('Magnitude of the transfer functions');
axis([50 500 0 1.1])
gtext('R=10');gtext('R=100');
gtext('R=1000');gtext('R=7816.4');
