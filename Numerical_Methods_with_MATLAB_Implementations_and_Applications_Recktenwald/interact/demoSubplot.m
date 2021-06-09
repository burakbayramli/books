% demoSubplot  Demonstration of subplot with four sine functions

x = linspace(0,2*pi);
subplot(2,2,1);
plot(x,sin(x));     axis([0 2*pi -1.5 1.5]);   title('sin(x)');

subplot(2,2,2);
plot(x,sin(2*x));   axis([0 2*pi -1.5 1.5]);   title('sin(2x)');

subplot(2,2,3);
plot(x,sin(3*x));   axis([0 2*pi -1.5 1.5]);   title('sin(3x)');

subplot(2,2,4);
plot(x,sin(4*x));   axis([0 2*pi -1.5 1.5]);   title('sin(4x)');	
