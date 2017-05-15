% The sinc function
x=-10:0.01:10;
y=sinc(x); %the sinc function
plot(x,y,'k'); %plots the function
hold on;
plot([-10 10],[0 0],'--k'); %horizontal dotted line
xlabel('x'); title('sinc function')
