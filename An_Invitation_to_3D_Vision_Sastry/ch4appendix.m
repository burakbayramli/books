close all;
x = 0 : 0.001 : 2*pi 
plot(x,0.5*sin(x + pi/8)+ 1);
axis([0.2, 2*pi, 0, 3]);  set(gca,['x','tick'],[]); set(gca,['y','tick'],[]);
xlabel(''); ylabel(''); box on;
% text(0.2,1,'f[x]');
figure; 
x = 0:0.4:2*pi;
stem(x,0.5*sin(x + pi/8)+ 1);
axis([0.2, 2*pi, 0, 3]);  set(gca,['x','tick'],[]); set(gca,['y','tick'],[]);
xlabel(''); ylabel(''); box on;
% text(0.2,1,'f[x]');

figure
x = -pi:0.01:pi; T = 0.3;
plot(x,sin(pi*x/T)./(pi*x/T));
set(gca,['x','tick'],[]); set(gca,['y','tick'],[]);
axis([-pi, pi, -1.1, 1.1]);
hold on; line([-pi pi],[0 0]);
figure;
x= -pi:0.01:pi; T = 0.3;
plot(x, ((x*(pi^2)/(T^2)).*cos(pi*x/T) - (pi/T)*sin(pi*x/T))./(pi*x/T).^2);
set(gca,['x','tick'],[]); set(gca,['y','tick'],[]);
hold on; line([-pi pi],[0 0]);
axis([-pi, pi, -5, 5]);