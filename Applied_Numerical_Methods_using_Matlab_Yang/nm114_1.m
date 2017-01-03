%nm114_1: plot the data of a 5x2 array stored in "temp.dat"
clear, clf
load temp.dat
subplot(221), plot(temp)
title('the highest/lowest temperature of these days')
ylabel('degrees[C]'), xlabel('day')
days=[11 12 14 15 17];
subplot(222), plot(days,temp)
axis([11 17 5 25])
pause, axis([10 20 0 30])
pause, grid on, hold on
pause, plot(days,temp(:,1),'b*',days,temp(:,2),'ro')
subplot(223)
plot(temp(:,1),temp(:,2),'bx')
hold on, plot(temp(:,1),temp(:,2),'b:')
subplot(224)
[temp1,I]= sort(temp(:,1));
temp2= temp(I,2);
pause, plot(temp1,temp2)