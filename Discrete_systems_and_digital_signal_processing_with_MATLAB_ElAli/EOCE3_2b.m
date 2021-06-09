clf % clear the screen
t=0:0.001:1; % the interval where v(t) is defined (T=1)
x=0.5;
for n = 1:2:7,% considering 4 terms (n odd)
x=x + (2/(n*pi))*sin(2*n*pi*t) ;
end
for n = 9:2:55,% considering 28 terms (n odd)
y=x + (2/(n*pi))*sin(2*n*pi*t) ;
end
subplot(2,1,1),plot(t,x),
title('Approximation using 4 terms'),
subplot(2,1,2), plot(t,y),
title('Approximation using 28 terms'),
xlabel('n'); 
