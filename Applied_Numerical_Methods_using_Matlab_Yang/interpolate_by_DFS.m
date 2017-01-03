%interpolate_by_DFS
clear, clf
w1=pi; w2=.5*pi; %two tones
N=32; n=[0:N-1]; T=0.1; t=n*T; 
x=sin(w1*t)+0.5*sin(w2*t)+(rand(1,N)-0.5); %0.2*sin(20*t);
ti=[0:T/5:(N-1)*T];
subplot(611), plot(t,x,'k.') %original data sequence
title('original sequence and interpolated signal')
[xi,Xi]=interpolation_by_DFS(T,x,1,ti);
hold on, plot(ti,xi,'r') %reconstructed signal
k=[0:N-1];
subplot(612), stem(k,abs(Xi),'k.') %original spectrum
title('original spectrum')
[xi,Xi]=interpolation_by_DFS(T,x,1/2,ti);
subplot(613), stem(k,abs(Xi),'r.') %filtered spectrum
title('filtered spectrum')
subplot(614), plot(t,x,'k.', ti,xi,'r') %filtered signal
title('filtered signal')
[xi,Xi]=interpolation_by_DFS(T,x,1/4,ti);
subplot(615), stem(k,abs(Xi),'r.') %filtered spectrum
title('filtered spectrum')
subplot(616), plot(t,x,'k.', ti,xi,'r') %filtered signal
title('filtered signal')
