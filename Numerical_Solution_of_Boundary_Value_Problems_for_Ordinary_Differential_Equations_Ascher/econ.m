function [p,f] = econ
% Import solution of econ

temp = load('econ.dat');
r = temp(1,1); d = temp(1,2);
s = temp(2,1); g = temp(2,2);
p = temp(3:end,1);
f = temp(3:end,2);

plot(p,f,'r','LineWidth',2)
title(['Solution with r = ',num2str(r),', \delta = ',num2str(d),...
       ', \sigma = ',num2str(s),', and \gamma = ',num2str(g),'.'])
xlabel('p')
ylabel('f(p)')

