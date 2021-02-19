function [x,y] = lubrication
% Import solution of lubrication

temp = load('lubrication.dat');
x = temp(:,1);
y = temp(:,2:end);

plot(x,y)
axis([-pi/2 pi/2 0 1.1])
title('Solution of the lubrication problem with \epsilon = 0.1.')
