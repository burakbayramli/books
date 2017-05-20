R=1000;
L=1000/(20*pi);
w=0:0.1:100*pi;
T1=w./(R/L+j*w);
Mag_T1=abs(T1);
plot(w,Mag_T1);
xlabel('w in rad');
ylabel('Magnitude of the transfer function');
gtext('----3db-down from max mag of 1 (0.707)');
