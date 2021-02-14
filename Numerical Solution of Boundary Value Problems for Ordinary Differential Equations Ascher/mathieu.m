function [x,y] = mathieu
% Import solution of mathieu

temp = load('mathieu.dat');
lambda = temp(1,1); 
x = temp(2:end,1);
y = temp(2:end,2);

plot(x,y)
axis([0 pi -1 1.1]);
title('Eigenfunction of Mathieu''s equation')
xlabel(['The fourth eigenvalue is approximately',num2str(lambda),'.'])
