%nm132_1: to plot the graph of (e^x-1)/x
clear
ss=sin(th(1));
for i=2:N
   ss= ss+sin(th(i));
end
toc, ss
tic
ss=sum(sin(th));
toc, ss

%Avoid a runtime error
x=[-100:100]/100;
f3=(exp(x)-1)./x;
plot(x,f3)


