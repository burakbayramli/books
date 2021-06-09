clf % to clear the screen
t=-pi:.05:pi;
y_for_n_1=.7854 - (2/pi)*(.7071)*cos(t-.7854)+.7071*cos(t-pi/2);
y_for_n_2=.7854-(2/pi)*(.7071)*cos(t-.7854)-(2/(9*pi))*.3162*cos(3*t-1.249)+.7071*cos(t-pi/2)-.4472*cos(2*t-pi/2-1.1071);
plot(t,y_for_n_1,'-',t,y_for_n_2,'go');
xlabel('time in seconds');
ylabel('y(t)');
legend('n=1','n=2',4);
