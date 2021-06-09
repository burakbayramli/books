t=0:0.05:10;
h1=exp(-t);
h2=exp(-t);
z=1-exp(-t);
y=1-exp(-t) - t.*exp(-t);
subplot(4,1,1), plot(t,h1),grid,
ylabel('h1(t)'),
subplot(4,1,2), plot(t,h2), grid,
ylabel('h2(t)'),
subplot(4,1,3), plot(t,z), grid,
ylabel('z(t)'), 
subplot(4,1,4), plot(t,y), grid,
xlabel(' time in seconds'), ylabel('y(t)'); 
