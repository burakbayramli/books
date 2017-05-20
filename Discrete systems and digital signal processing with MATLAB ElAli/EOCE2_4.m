t=0:0.05:10;	%establishing a range for time
h1=exp(-t);
h2=exp(-t);
z1=1-exp(-t);
z2=1-exp(-t);
y=2-2*exp(-t);
subplot(5,1,1), plot(t,h1), grid
ylabel(' h1(t)'),
%a plot of five rows with grids
subplot(5,1,2), plot(t,h2), grid	
ylabel(' h2(t)'),
subplot(5,1,3), plot(t,z1), grid
ylabel(' z1(t)'),
subplot(5,1,4), plot(t,z2), grid
ylabel(' z2(t)'),
subplot(5,1,5), plot(t,y), grid
xlabel(' time in seconds'), ylabel(' y(t)');
