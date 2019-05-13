x = [-2:0.1:2];
y = (x.^3 + (sin(x)).^2).*exp(-abs(x));
figure(1)
plot(x,y,'linewidth',3);
xlabel('x: Independant variable');
ylabel('y: Dependant variable');
title('Grapgh of y = (x^3 + sin^2(x))exp(-|x|)');
axis([-2 2 -1 1.5]);

y2 = (exp(-(x+0.5).^2) - 1);
y3 = exp(-0.4*(x-2).^2);
y4 = y2 + 2*y3;
figure(2)
plot(x,y,'b-',x,y2,'r--',x,y3,'k--',x,y4,'b--','linewidth',3);
legend('y: original', 'y2', 'y3', 'y4: approximated');
xlabel('x: Independant variable');
ylabel('y, y2, y3, y4');
title('Graphs of y = (x^3 + sin^2(x))exp(-|x|), approximation, y4 = y2 + 2y3');
axis([-2 2 -1 1.5]);